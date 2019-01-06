package com.thirdship.libunit

/**
 * Represents a BaseUnit that does not require a conversion system
 *
 * @example
 * {{{
 *     val a = new InconvertibleUnit("bob")
 *     val b = new InconvertibleUnit("bob")
 *     a.equals(b) 			// true
 *     println(a) 			// bob
 *     println(a.unitTag) 	// InconvertibleUnit#bob
 * }}}
 *
 * @param name is the label to apply to the unit
 */
class InconvertibleUnit(val name: String) extends BaseUnit {

	override def defaultUnit(): BaseUnit = new InconvertibleUnit(name)

	override def conversionFunction(unit: BaseUnit): Double => Double = unit match {
		/*
			If the unit is an InconvertibleUnit, and because we know that the two units are convertible, the units must be the same.
			Thus, we must return a no-op.
		*/
		case _: InconvertibleUnit => (a: Double) => a

		// If the unit is a ComputableUnit, then punt the problem over to it.
		case u: CompoundUnit => u.conversionFunction(this)
		case _ => throw new InvalidConversionState(this, unit)
	}

	override def isConvertible(unit: BaseUnit): Boolean = unit match {
		// The only case two InconvertibleUnits are equal is when they share the same name
		case unit: InconvertibleUnit => unit.name.equals(name)

		// If the unit is a ComputableUnit, we as unit as it knows more than we do.
		case u: CompoundUnit => u.isConvertible(this)
		case _ => false
	}

	override def toString: String = name

	override def equalUnits(unit: BaseUnit): Boolean = unit match {
		// The only case two InconvertibleUnits are equal is when they share the same name
		case u: InconvertibleUnit => u.name.equals(name)

		// If the unit is a ComputableUnit, we as unit as it knows more than we do.
		case u: CompoundUnit => u.equalUnits(this)
		case _ => false
	}

	override def unitHashCode: Int = name.hashCode

	override def getUnitName: String = name

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: BaseUnit] =
		if (! str.isEmpty && str.equalsIgnoreCase(name)) {
			Some(new InconvertibleUnit(name))
		} else {
			None
		}
}
