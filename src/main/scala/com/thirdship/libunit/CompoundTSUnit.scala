package com.thirdship.libunit

import com.thirdship.libunit.units.ScalarTSUnit
import com.thirdship.libunit.utils.ParenUtils

/**
 * A fraction of units.
 *
 * This class holds two lists of TSUnits, the numerator and the denominator. These two lists are kept separate, but
 * they are operated on as if they are actually dividing each other.
 *
 * Additionally, there is a boolean to represent if the fraction is simplified. We defined this state as being when
 * there are no units in the numerator that can convert to units in the denominator. This means that fractions like m/m
 * are not considered simple. Nothing prevents the construction of an unsimplified fraction, but every operation that
 * could create one attempts to simplify the fraction afterwards.<br>
 *
 * Finally, there is a scalar that helps simplify units by allowing for things like km/m to become (1000) Scalar. One
 * thing to be careful of is that a ComputableTSUnit can be "simplified", but have a scalar that is not 1. This allows
 * for an easier internal simplification mechanism, and when wrapped properly should not pose an issue.
 *
 * @note Think of the arguments as: Is "scalar (numerator / denominator)" simplified?
 * @note It is faster to store a simplified ComputableTSUnit as if it is not simplified, before every equals operation
 *       it will be re-simplified. This could be slow in some situations.
 * @example
 * {{{
 * 	   val m = new LengthTSUnit("m")
 * 	   val km = new LengthTSUnit("km")
 * 	   val s = new TimeTSUnit("s")
 *
 *     val a = new ComputableTSUnit(List(m), List(s))
 *     val b = new ComputableTSUnit(List(m,m,s), List(m,s,s))
 *     a.equals(b) 			// true
 *     println(a) 			// "m / s"
 *     println(a.unitTag)	// "ComputableTSUnit#(LengthTSUnit#m)/(TimeTSUnit#s)"
 *
 *     val c = new ComputableTSUnit(List(km), List(s))
 *     a.equals(c)			// false
 *     a.isConvertible(c)	// true
 *     println(c.unitTag)	// "ComputableTSUnit#(LengthTSUnit#m)/(TimeTSUnit#s)"
 * }}}
 * @param numerator a list of TSUnits that represent the numerator of the fraction
 * @param denominator a list of TSUnits that represent the denominator of the fraction
 * @param simplified if the fraction is simplified.
 * @param scalar considered to be a multiplier on the fraction.
 */
class CompoundTSUnit(val numerator: List[TSUnit] = List.empty[TSUnit],
                     val denominator: List[TSUnit] = List.empty[TSUnit],
                     private val simplified: Boolean = false,
                     val scalar: ScalarTSUnit = new ScalarTSUnit()
						  ) extends TSUnit {

	// scalastyle:off method.name
	override def *(unit: TSUnit): TSUnit = unit match {
		/*
			If the unit is a ComputableTSUnit we need to add the numerators and the denominators to each other,
			and we also need to update the scalar. Finally, we need to simplify the new unit.

			 a     c     a * c
			--- * --- = -------
			 b     d     b * d
		 */
		case unit: CompoundTSUnit => new CompoundTSUnit(
			numerator.foldLeft(unit.numerator)((list, u) => list.::(u)),		// combine numerators
			denominator.foldLeft(unit.denominator)((list, u) => list.::(u)),	// combine denominators
			false, 																// the fraction needs to simplified
			new ScalarTSUnit(scalar.value*unit.scalar.value)					// multiply scalars
		).simplifyType

		// If it is any other ts unit, just add the unit to the numerator
		case unit: TSUnit => new CompoundTSUnit(numerator.::(unit), denominator, false, scalar).simplifyType
	}


	override def /(unit: TSUnit): TSUnit = unit match {
		/*
			If the unit is a ComputableTSUnit, we need to logically divide them. We need to add the numerators to the
			respective denominator. Then we need to update the scalar, and attempt to simplify.

			 a     c     a     d     a * d
			--- / --- = --- * --- = -------
			 b     d     b     c     b * c
		 */
		case unit: CompoundTSUnit => new CompoundTSUnit(
			numerator.foldLeft(unit.denominator)((list, u) => list.::(u)),	// combine this.numerator to unit.denominator
			denominator.foldLeft(unit.numerator)((list, u) => list.::(u)),	// combine this.denominator to unit.numerator
			false,															// the fraction needs to simplified
			new ScalarTSUnit(scalar.value/unit.scalar.value)				// multiply scalars
		).simplifyType

		// If it is any other ts unit, just add the unit to the denominator
		case unit: TSUnit => new CompoundTSUnit(numerator, denominator.::(unit))
	} // scalastyle:on method.name

	override def conversionFunction(unit: TSUnit): (Double) => Double = {
		// If this is not simplified, we cannot guarantee our algorithms can work. Thus, we must simplify first.
		if (! simplified) {
			return simplifyType.convert(unit)
		}

		/*
			Since we know that we are simplified and that we have passed the isConvertible check, we can begin
			converting units. Because both of those things are true, we know that we are in one of two cases.
		 */
		unit match {

			/*
				The other unit is also a ComputableTSUnit. Thus, we need to generate conversion functions for the
				numerator, denominator, and the scalar. Then, we must combine them.
			 */
			case u: CompoundTSUnit => if (u.simplified) {	// ensure the other unit is simplified first
				val n = generateConversionFunction(numeratorDimensions, u.numeratorDimensions)
				val d = generateConversionFunction(denominatorDimensions, u.denominatorDimensions)
				val s = scalar.conversionFunction(u.scalar)
				// returns a function for the program to run later
				(a: Double) => s(n(a) / d(1))
			} else conversionFunction(u.simplifyType)

			/*
				If the other unit is not a computable unit, and this cannot be simplified further, then they are not
				convertible unless the scalar is not 1.
			 */
			case u: TSUnit => if (scalar.value != 1) {
				val s = scalar.convert(new ScalarTSUnit())
				val n = numerator.head.convert(u)

				// returns a function for the program to run later
				(a: Double) => s(n(a))
			} else throw new InvalidConversionState(this, unit);
		}
	}

	override def isConvertible(unit: TSUnit): Boolean = {
		// If the we are not simplified, we cannot guarantee our algorithms can work. Thus, we must simplify first.
		if (! simplified) {
			return simplifyType.isConvertible(unit)
		}

		/*
			Because we know that we are simplified, we can assume that the other needs to be a CompoundTSUnit,
			or we are dealing with an internal scalar.
		 */
		unit match {

			/*
				If the other unit is a CompoundTSUnit as well, we must see if their dimensions are the same.
			 */
			case u: CompoundTSUnit => isCompoundConvertible(u)

			/*
				If the scalar is not 1, then there is the real possibility of the units being convertible, but needing
				a scalar modification. We check this by ensuring that there is only one unit in the fraction, and that
				it happens to be in the numerator. But, it still has to be convertible.
			 */
			case u: TSUnit =>
        scalar.value != 1 &&
          numerator.length == 1 &&
          denominator.isEmpty &&
          numerator.head.isConvertible(u)
		}
	}

  private def isCompoundConvertible(unit: CompoundTSUnit): Boolean = {
    if (! simplified) {
      false // This method relies on this being simplified
    } else if (unit.simplified) { // The other unit needs to be simplified as well
      if (numerator.length == unit.numerator.length && denominator.length == unit.denominator.length) {
        numeratorDimensions.keySet.equals(unit.numeratorDimensions.keySet) &&
          denominatorDimensions.keySet.equals(unit.denominatorDimensions.keySet)
      } else false
    } else isConvertible(unit.simplifyType) // Self-calling might result in comparing a CompoundTSUnit to not a CompoundTSUnit
  }

  override def toString: String = {
		val s = scalar.value.toString

		val n = if (numerator.isEmpty) { // simply set the numerator to the scalar value
			s
		} else { // if the scalar is not 1, display it with a rendering of the numerator
			(if (scalar.value != 1) s + " " else "") +
			numerator
				.map(_.toString)
				.sorted
        .mkString(" ")
		}

		// if there is nothing in the denominator pretend that it does not exist
		if (denominator.isEmpty) return n

		// render the denominator
		val d = denominator
			.map(_.toString)
			.sorted
			.mkString(" ")

	  n + " / " + d
	}

	override def equalUnits(unit: TSUnit): Boolean = {
		// If the we are not simplified, we cannot guarantee our algorithms can work. Thus, we must simplify first.
		if (! simplified) {
			return simplifyType.equalUnits(unit)
		}

		unit match {

			// if the other unit is a ComputableTSUnit, we need to equate the unit lists
			case u: CompoundTSUnit => if (u.simplified) {equateUnitLists(u)} else equalUnits(u.simplifyType)

			/*
				If the other unit can be simplified into something simpler than a ComputableTSUnit, and this cannot,
				then they are not equal. They may still be convertible into each other. For example, this might have
				a scalar applied.
			*/
			case u: TSUnit => false
		}
	}

	override def unitHashCode: Int = {
		// If the we are not simplified, we cannot guarantee our algorithms can work. Thus, we must simplify first.
		if (! simplified) simplifyType.unitHashCode
		else numerator.map(_.hashCode).sum + denominator.map(_.hashCode).sum
	}

	// simply swap the numerator with the denominator and invert the scalar.
	override def inverse: CompoundTSUnit = new CompoundTSUnit(denominator, numerator, simplified, new ScalarTSUnit(1 / scalar.value))

	override def getUnitName: String = {
		"(" +
		numerator.foldLeft("")((s, u) => s + u.unitTag) +	// collapse the numerator
		")/(" +
		denominator.foldLeft("")((s, u) => s + u.unitTag) +	// collapse the denominator
		")"
	}


	/**
	 * Holds the numerator in a more optimal destruction for simplification purposes.
	 */
	lazy val numeratorDimensions = getDimensionsFromList(numerator)


	/**
	 * Holds the numerator in a more optimal destruction for simplification purposes.
	 */
	lazy val denominatorDimensions = getDimensionsFromList(denominator)


	/**
	 * Creates a Map of unitTag to a List of TSUnits that have that unitTag
	 *
	 * {{{list.sortBy(_.unitTag).groupBy(_.unitTag)}}}
	 *
	 * @note unitTags being equal should mean that they are convertible.
	 *       However, this may not be the case if an error is made in a TSUnit.
	 * @param list a list of TSUnits to use
	 * @return the Map[String, List[TSUnit] ]
	 */
	private def getDimensionsFromList(list: List[TSUnit]) = list.sortBy(_.unitTag).groupBy(_.unitTag)


	/**
	 * Creates a conversion function from one unit dimension map to another
	 *
	 * This function ingests the dimensions of the numerator or denominator of any two ComputableTSUnits and will output
	 * a single function that would convert from one to the other.
	 *
	 * @param from is the one of the dimensions of the source TSUnit
	 * @param to is the the same dimension as 'from', but the target TSUnit instead
	 * @return a function mapping a value in from's units to a value in to's units
	 */
	private def generateConversionFunction(from: Map[String, List[TSUnit]], to: Map[String, List[TSUnit]]): (Double) => Double = {

		// In each row in the from map
		val ret = from.map(e => {
			// Check to see if it exists in the to map
			if (to.contains(e._1)) {
				/*
					If it does, then we need to remove duplicates. This is required so that we don't waste precision.
					Each map stores a list of TSUnits, so we need to compare the lists in side of the maps.
				 */
				val changes = e._2.zip(to(e._1)) 	// Zip
					.filterNot(s => s._1.equals(s._2))		// Remove duplicates
					.map(u => u._1.convert(u._2))			// Create the conversion function
					.foldLeft((a: Double) => a)((chain, fun) => chain.compose(fun))	// Reduce the functions into each other

				// Add the result to the list we are crafting.
				// Note, at this point changes is a function (Double) => Double
				changes
			} else {
				throw new InvalidConversion(e._1 + " is not in target dimensions " + to.keySet.foldLeft("")((s, st) => s + "," + st))
			}

		/*
			Now that we have all of the conversion functions on like unit tags, we need to merge them into a single
			master conversion function.
		 */
		}).foldLeft((a: Double) => a)((chain, fun) => chain.compose(fun)) // Reduce the functions into each other

		// We now return the created function, a single function, that will (eventually) convert from 'from' to 'to'.
		ret
	}

	/**
	 * @return this or a new TSUnit that is simpler in type.
	 * @note We define simpler to mean that it requires less information to fully define the unit.
	 * @example <ul>
	 *          <li>m km s / km s min --> m / min</li>
	 *          <li>m/m --> 1 Scalar</li>
	 *          <li>km/m --> 1000 Scalar. The scalar should be removed by the user.
	 *          <br>([[com.thirdship.libunit.TSUnitValuePair]] does this automatically)</li>
	 *          </ul>
	 */
	private[libunit] def simplifyType: TSUnit = {

		// If this is already simplified then, we must trust ourselves. (You can do it you!)
		if (simplified) {
			return this
		}

		// If there is nothing in the fraction, then there is only a scalar left. Return it, go home.
		if (numerator.isEmpty && denominator.isEmpty) {
			return scalar
		}

		// Get all the ComputableTSUnits in the fraction
		val oldGoodNumUnits = numerator.filterNot(_.isInstanceOf[CompoundTSUnit])
		val oldGoodDenUnits = denominator.filterNot(_.isInstanceOf[CompoundTSUnit])

		// If there are ComputableTSUnits, we need to combine them into one
		if (oldGoodNumUnits != numerator || oldGoodDenUnits != denominator) {
			return simplifyNestedCompoundTSUnits(oldGoodNumUnits, oldGoodDenUnits)
		}

		simplifyNestedScalarTSUnits
	}

	private def simplifyNestedScalarTSUnits: TSUnit = {
		// Get all the scalars in the fraction
		val n_scalars = numerator.filter(u => u.isInstanceOf[ScalarTSUnit])
		val d_scalars = denominator.filter(u => u.isInstanceOf[ScalarTSUnit])

		// If there are scalars, we need to combine the scalars into the root scalar
		if (n_scalars.nonEmpty || d_scalars.nonEmpty) {
			val top_scalar = if (n_scalars.isEmpty) scalar.value
			else scalar.value * n_scalars.map(_.asInstanceOf[ScalarTSUnit].value).product

			val bottom_scalar = if (d_scalars.isEmpty) 1
			else n_scalars.map(_.asInstanceOf[ScalarTSUnit].value).product

			val s = new ScalarTSUnit(top_scalar / bottom_scalar)
			val n = numerator diff n_scalars
			val d = denominator diff d_scalars
			new CompoundTSUnit(n, d, scalar = s).simplifyType
		}
		else if (scalar.value == 1 && numerator.length == 1 && denominator.isEmpty) {
			numerator.head
		} else simplifyConvertibleUnits()
	}

	private def simplifyNestedCompoundTSUnits(oldGoodNumUnits: List[TSUnit], oldGoodDenUnits: List[TSUnit]): TSUnit = {
		// Identify the new numerator
		val numNumUnits = numerator
			.filter(_.isInstanceOf[CompoundTSUnit])
			.map(_.asInstanceOf[CompoundTSUnit])
			.flatMap(_.numerator)
		val denDenUnits = denominator
			.filter(_.isInstanceOf[CompoundTSUnit])
			.map(_.asInstanceOf[CompoundTSUnit])
			.flatMap(_.denominator)
		val newNumerator = oldGoodNumUnits ++ numNumUnits ++ denDenUnits

		// Identify the new denominator
		val numDenUnits = numerator
			.filter(_.isInstanceOf[CompoundTSUnit])
			.map(_.asInstanceOf[CompoundTSUnit])
			.flatMap(_.denominator)
		val denNumUnits = denominator
			.filter(_.isInstanceOf[CompoundTSUnit])
			.map(_.asInstanceOf[CompoundTSUnit])
			.flatMap(_.numerator)
		val newDenominator = oldGoodDenUnits ++ numDenUnits ++ denNumUnits

		// Identify the new scalar
		val numScalars = numerator
			.filter(_.isInstanceOf[CompoundTSUnit])
			.map(_.asInstanceOf[CompoundTSUnit])
			.foldRight(new ScalarTSUnit)((numUnit, nextScalar) =>
				(numUnit.scalar * nextScalar).asInstanceOf[ScalarTSUnit])
		val denScalars = denominator
			.filter(_.isInstanceOf[CompoundTSUnit])
			.map(_.asInstanceOf[CompoundTSUnit])
			.foldRight(new ScalarTSUnit)((denUnit, nextScalar) =>
				(denUnit.scalar * nextScalar).asInstanceOf[ScalarTSUnit])
		val newScalar = (scalar * numScalars / denScalars).asInstanceOf[ScalarTSUnit]

		new CompoundTSUnit(newNumerator, newDenominator, scalar = newScalar).simplifyType
	}

	/**
	 * @param u the other ComputableTSUnit to check against
	 * @return true if the two ComputableTSUnits are the same.
	 */
	private def equateUnitLists(u: CompoundTSUnit): Boolean = {
		/*
     First, we check to see if the units can be converted between each other. This verifies list lengths and dimension sets.
     */
    if (isConvertible(u)) {
      /*
     In order for the ComputableTSUnits to be equal, the this list cannot contain elements not in u and vice versa, for both numerators and denominators.
     */

      val numsLeft = numerator.diff(u.numerator)
      val numsRight = u.numerator.diff(numerator)
      val densLeft = denominator.diff(u.denominator)
      val densRight = u.denominator.diff(denominator)

      numsLeft.isEmpty &&
        numsRight.isEmpty &&
        densLeft.isEmpty &&
        densRight.isEmpty
		} else false

	}

	/**
	 * @note If a ComputableTSUnit is returned at this point, then we have verified that the TSUnit should be a
	 *       ComputableTSUnit and that it is as simple as it can be. This means that it may have a non 1 scalar, that
	 *       need to be taken care of by the user. That said, our internal process will take into account this scalar,
	 *       but we recommend taking care of this scalar.
	 * @return
	 */
	private def simplifyConvertibleUnits(): TSUnit = {
		// Check to see if the numerator and the denominator talk about the same types of units
		val numSet = numeratorDimensions.keys.toSet
		val denSet = denominatorDimensions.keys.toSet
		val cancelableUnitTypes = numSet.intersect(denSet).toSeq
		val remainingNumUnits = (numSet -- denSet).toSeq.flatMap(numeratorDimensions)
		val remainingDenUnits = (denSet -- numSet).toSeq.flatMap(denominatorDimensions)

		/* If there is a Length for example in both the numerator and the denominator, then we know that the fraction is
			simplified. Thus we need to cancel out a Length from both, this may result in a scalar, so we must account
			for that as well.
		*/
		if (cancelableUnitTypes.nonEmpty) {
			/*
				This is the case that there is a m/km or something, this set the scalar to the conversion ratio and pick on.
				m/km is 1m/1000m so the scalar would be 1/1000 and the result would be just scalar.
			 */
			var canceledUnitsCoefficient = 1d
			// For every unit tag that is shared between the two, loop over and simplify
			val (reducedNumerator, reducedDenominator) = cancelableUnitTypes
				.map(ut => {
					// get the un simplified numerator and denominator that refer to this unit tag.
					val numUnits = numeratorDimensions(ut)
					val denUnits = denominatorDimensions(ut)

					// take the first element in the list
					val (firstNum, remainingNumUnits) = (numUnits.head, numUnits.tail)
					val (firstDen, remainingDenUnits) = (denUnits.head, denUnits.tail)

					// then convert the difference
					canceledUnitsCoefficient *= firstNum.convert(firstDen, 1)

					// and return the new lists to recurse on later
					(remainingNumUnits, remainingDenUnits)
				})
				.unzip

			// Flatten the tuple of lists we made into two lists of ts units.
			val newNumerator: Seq[TSUnit] = remainingNumUnits ++ reducedNumerator.flatten
			val newDenominator: Seq[TSUnit] = remainingDenUnits ++ reducedDenominator.flatten

			// Make a new Computable ts unit and attempt to simplify it
			return new CompoundTSUnit(newNumerator.toList, newDenominator.toList, false, new ScalarTSUnit(scalar.value * canceledUnitsCoefficient)).simplifyType
		}
		// We have passed all simplification steps, so then we must set the simply flag.
		new CompoundTSUnit(numerator, denominator, true, scalar)
	}

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] = {
		// replace all forms of braces with parenthesis.
		val str_all_parens = ParenUtils.cleanParenTypes(str)

		// remove duplicate parenthesis
		val str_no_duplicate_parens = ParenUtils.removeDuplicates(str_all_parens)

		val parsed = parseRecursive(str_no_duplicate_parens, currentUnitParser)


		parsed
	}


	private def parseRecursive(str: String, currentUnitParser: UnitParser): Option[_ <: TSUnit] = {
		if (str.length <= 0) return None

		// find first param

		val first_paren = str.indexOf("(")
//		val last_paren = str.indexOf(")")

		val units: List[TSUnit] = parseUnitList(str, currentUnitParser, first_paren)

		if (units.nonEmpty) {
			val (compoundUnits, otherUnits) = units.partition {
				case _: CompoundTSUnit => true
				case _ => false
			}
			val compoundUnitsProduct = compoundUnits.reduceOption(_ * _).getOrElse(new ScalarTSUnit())
			val otherUnitsProduct = otherUnits.reduceOption(_ * _).getOrElse(new ScalarTSUnit())
			val out_unit = compoundUnitsProduct * otherUnitsProduct

			Some(out_unit)
		} else {
			None
    }
	}


  private def parseUnitList(str: String, currentUnitParser: UnitParser, first_paren: Int): List[TSUnit] = {
    if (first_paren >= 0) {
      val before = str.substring(0, first_paren).trim
      val matching_paren = ParenUtils.findMatchingParen(str, first_paren).getOrElse(str.length - 1)
      val selection = str.substring(first_paren + 1, matching_paren)
      val rest = str.substring(matching_paren + 1, str.length).trim
      if (before.endsWith("/")) {
        val before_unit = currentUnitParser.parse(before.substring(0, before.length - 1))
        val selection_unit = currentUnitParser.parse(selection)
        val rest_unit = currentUnitParser.parse(rest) // TODO handle (a b)^2 --> a a b b, a (b c)^-1 --> a/ b c
        val rel = if (before_unit.isDefined) {
          Some(before_unit.get / selection_unit.getOrElse(new ScalarTSUnit()))
        } else None
        List(rel, rest_unit).flatten
      } else if (rest.startsWith("/")) {
        val before_unit = currentUnitParser.parse(before)
        val selection_unit = currentUnitParser.parse(selection)
        val rest_unit = currentUnitParser.parse(rest.substring(1)) // TODO handle (a b)^2 --> a a b b, a (b c)^-1 --> a/ b c
        val rel = if (selection_unit.isDefined) {
          Some(selection_unit.get / rest_unit.getOrElse(new ScalarTSUnit()))
        } else None
        List(before_unit, rel).flatten
      } else {
        val before_unit = currentUnitParser.parse(before)
        val selection_unit = currentUnitParser.parse(selection)
        val rest_unit = currentUnitParser.parse(rest) // TODO handle (a b)^2 --> a a b b, a (b c)^-1 --> a/ b c
        List(before_unit, selection_unit, rest_unit).flatten
      }
    } else {
      // May contain * / ' ' or more than one concatenated.
      val divisor = str.indexOf("/")
      if (divisor >= 0) {
        val numerator = str.substring(0, divisor)
        val denominator = str.substring(divisor + 1, str.length)
        val numerator_unit: Option[TSUnit] = currentUnitParser.parse(numerator)
        val denominator_unit: Option[TSUnit] = currentUnitParser.parse(denominator)
        if (numerator_unit.isDefined && denominator_unit.isDefined) {
          List(numerator_unit.get / denominator_unit.get)
				} else List.empty
      } else {
        // We can assume that there are zero, one, or more units.
        // TODO fix the case of mph, ftlbs ect...
        val list = str.split("""[\*\s]""").toList.filter(s => s.nonEmpty)
        // perform ^ operation, m^2^2s^2 --> m m m m s s
        val power_applied: List[String] = applyPower(list)
        if (power_applied.length <= 1) List.empty
        else power_applied.flatMap(currentUnitParser.parse)
      }
    }
  }

  private def applyPower(list: List[String]): List[String] = {
    list.flatMap(s => {
      val groupings = """[a-zA-Z]+(\^\-*[0-9]+)+""".r.findAllIn(s).toList

      if (groupings.isEmpty) {
        List(s)
			} else {
        groupings.flatMap(g => {
          val arr = g.split("""\^""").toList
          val unit = arr.head
          val power = arr.drop(1)
            .map(n => try {
              Integer.parseInt(n).toDouble
            } catch {
              case _: Exception => 0d
            })
            .foldRight(1d)((pow, out) =>
              Math.pow(pow, out)
            )
            .toInt

          var ret = List.empty[String]
          for (i <- 1 to Math.abs(power))
            ret = ret.::(unit)

          if (power >= 0) {
            ret
					} else {
            ret.map(u => "1/" + u)
					}
        })
      }
    })
  }

  override def defaultUnit(): TSUnit = {
		val n = numerator.map(u => u.defaultUnit())
		val d = denominator.map(u => u.defaultUnit())
		new CompoundTSUnit(n, d, simplified)
	}
}
