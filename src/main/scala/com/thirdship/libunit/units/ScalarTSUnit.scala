package com.thirdship.libunit.units

import com.thirdship.libunit._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Scalars.
 */
object ScalarHelpers {
	object Scalar {
		def apply(value: Double) = UnitValuePair(value, new ScalarTSUnit)
	}
}

/**
 * A Base Unit that defines a unitless scalar.
 *
 * @note this type of unit is used to represent when a conversion has taken place and resulted in a scalar and also
 *       also, it is used to represent when a unit no longer has any units.
 *
 * @param value the amount to scale by
 */
class ScalarTSUnit(val value: Double = 1) extends BaseTSUnit("Scalar"){

	override def *(unit: TSUnit): TSUnit = unit match {
		// We need to replace the current scalar instead of squaring a scalar.
		case u: ScalarTSUnit => new ScalarTSUnit(value * u.value)

		/*
			However, if it is not a Scalar, let the normal process apply.
			This makes sense as if the scalar is not 1, then we need to apply that scalar to the resulting fraction.
			Once the scalar is applied to the fraction, it can be pulled out and further simplified by UnitValuePair
		*/
		case u: TSUnit => super.*(u)
	}

	override def /(unit: TSUnit): TSUnit = unit match {
		// We need to replace the current scalar instead of squaring a scalar.
		case u: ScalarTSUnit => new ScalarTSUnit(value / u.value)

		/*
			However, if it is not a Scalar, let the normal process apply.
			This makes sense as if the scalar is not 1, then we need to apply that scalar to the resulting fraction.
			Once the scalar is applied to the fraction, it can be pulled out and further simplified by UnitValuePair
		*/
		case u: TSUnit => super./(u)
	}

	override def conversionFunction(unit: TSUnit): (Double) => Double = unit match {
			case u: ScalarTSUnit => (a: Double) => (a * value) / u.value
			case u: TSUnit => throw new InvalidConversionState(this,unit)
		}

	override def toString: String = if(value == 1) "Scalar" else value+"x"

	override def equalUnits(unit: TSUnit): Boolean = unit match {
		case u: ScalarTSUnit =>	name.equals(u.name) && value == u.value
		case u: TSUnit =>	super.equalUnits(u)
	}

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] = {
		if (str.isEmpty || str.equalsIgnoreCase(name))
			Some(new ScalarTSUnit())
		else
			None
	}
}
