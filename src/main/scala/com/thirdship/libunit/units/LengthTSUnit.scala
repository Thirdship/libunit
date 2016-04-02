package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit.{TSUnit, UnitValuePair, Conversion, ConvertableTSUnit}

import com.thirdship.libunit.utils.Helpers._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Lengths.
 */
object LengthHelpers{
	object Meters     { def apply(value: Double = 1) = UnitValuePair(value, new LengthTSUnit("m" )) }
	object Kilometers { def apply(value: Double = 1) = UnitValuePair(value, new LengthTSUnit("km")) }
}

/**
 * A convertible unit that measures the length of something.
 *
 * @example
 * 		new LengthTSUnit("m") would measure meters
 * 		new LengthTSUnit("km") would measure kilometers
 *
 * @param unit the name of the unit
 */
class LengthTSUnit(unit: String = "m") extends ConvertableTSUnit("Length", unit, "m"){
	override val conversionMap: Map[ExactString, Conversion[Double, Double]] = Map(
		baseUnit.i -> 1.asScalarFn,
		"km".i -> 1000.asScalarFn
	)

	override protected def getTSUnit(str: String): TSUnit = {
		new LengthTSUnit(str)
	}

	override val parseMap: Map[ExactString, String] = generateParseMap(Map(
		List("meter".w) -> "m",
		List("kilometer".w) -> "km"
	))
}