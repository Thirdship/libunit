package com.thirdship.libunit

/**
 * Represents a TSUnit that does not require a conversion system
 *
 * @example
 * {{{
 *     val a = new BaseTSUnit("bob")
 *     val b = new BaseTSUnit("bob")
 *     a.equals(b) 			// true
 *     println(a) 			// bob
 *     println(a.unitTag) 	// BaseTSUnit#bob
 * }}}
 *
 * @param name is the label to apply to the unit
 */
class BaseTSUnit(val name: String) extends TSUnit{

	override def conversionFunction(unit: TSUnit): (Double) => Double = unit match {
		/*
			If the unit is a BaseTSUnit, and because we know that the two units are convertable, the units must be the same.
			Thus, we must return a no-op.
		*/
		case u: BaseTSUnit => (a: Double) => a

		// If the unit is a ComputableTSUnit, then punt the problem over to it.
		case u: ComputableTSUnit => u.conversionFunction(this)
		case _ => throw new InvalidConversionState(this, unit)
	}

	override def isConvertible(unit: TSUnit): Boolean = unit match {
		// The only case two BaseTSUnits are equal is when they share the same name
		case unit: BaseTSUnit => unit.name.equals(name)

		// If the unit is a ComputableTSUnit, we as unit as it knows more than we do.
		case u: ComputableTSUnit => u.isConvertible(this)
		case _ => false
	}

	override def toString: String = name

	override def equalUnits(unit: TSUnit): Boolean = unit match {
		// The only case two BaseTSUnits are equal is when they share the same name
		case u: BaseTSUnit => u.name.equals(name)

		// If the unit is a ComputableTSUnit, we as unit as it knows more than we do.
		case u: ComputableTSUnit => u.equalUnits(this)
		case _ => false
	}

	override def getUnitName: String = name

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] =
		if(!str.isEmpty && str.equalsIgnoreCase(name))
			Some(new BaseTSUnit(name))
		else
			None
}
