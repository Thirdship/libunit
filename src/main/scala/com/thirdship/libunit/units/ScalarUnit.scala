package com.thirdship.libunit.units

import com.thirdship.libunit._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Scalars.
 */
object ScalarHelpers {
	object Scalar {
		def apply(value: Double): UnitValuePair = UnitValuePair(value, new ScalarUnit)
	}
}

/**
 * A Base Unit that defines a unit-less scalar.
 *
 * @note this type of unit is used to represent when a conversion has taken place and resulted in a scalar and also
 *       also, it is used to represent when a unit no longer has any units.
 *
 * @param value the amount to scale by
 */
class ScalarUnit(val value: Double = 1) extends InconvertibleUnit("Scalar") {
	override def defaultUnit(): BaseUnit = new ScalarUnit()

	// scalastyle:off method.name
	override def *(unit: BaseUnit): BaseUnit = unit match {
		// We need to replace the current scalar instead of squaring a scalar.
		case u: ScalarUnit => new ScalarUnit(value * u.value)

		/*
			However, if it is not a Scalar, let the normal process apply.
			This makes sense as if the scalar is not 1, then we need to apply that scalar to the resulting fraction.
			Once the scalar is applied to the fraction, it can be pulled out and further simplified by UnitValuePair
		*/
		case u: BaseUnit => super.*(u)
	}

	override def /(unit: BaseUnit): BaseUnit = unit match {
		// We need to replace the current scalar instead of squaring a scalar.
		case u: ScalarUnit => new ScalarUnit(value / u.value)

		/*
			However, if it is not a Scalar, let the normal process apply.
			This makes sense as if the scalar is not 1, then we need to apply that scalar to the resulting fraction.
			Once the scalar is applied to the fraction, it can be pulled out and further simplified by UnitValuePair
		*/
		case u: BaseUnit => super./(u)
	}
	// scalastyle:on method.name

	override def conversionFunction(unit: BaseUnit): (Double) => Double = unit match {
		case u: ScalarUnit => (a: Double) => (a * value) / u.value
		case _ => throw new InvalidConversionState(this, unit)
	}

	override def toString: String = if (value == 1) "Scalar" else value + "x"

	override def equalUnits(unit: BaseUnit): Boolean = unit match {
		case u: ScalarUnit => name.equals(u.name) && value == u.value
		case u: BaseUnit =>	super.equalUnits(u)
	}

	override def unitHashCode: Int = value.hashCode

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: BaseUnit] = {
		if (str.isEmpty || str.equalsIgnoreCase(name)) {
			Some(new ScalarUnit())
		} else None
	}
}
