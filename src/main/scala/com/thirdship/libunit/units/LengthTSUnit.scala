package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit.{TSUnit, TSUnitValuePair, Conversion, ConvertableTSUnit}

import com.thirdship.libunit.utils.Helpers._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Lengths.
 */
object LengthHelpers{
	object Meters     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("m" )) }
	object Kilometers 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("km")) }

	object Feet     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("ft" )) }
	object Inches 		{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("in")) }
}

/**
 * A convertible unit that measures the length of something.
 *
 * @example
 * 		new LengthTSUnit("m") would measure meters
 * 		new LengthTSUnit("km") would measure kilometers
  * @param unit the name of the unit
 */
class LengthTSUnit(unit: String = "m") extends ConvertableTSUnit("Length", unit, "m"){
	override val conversionMap: Map[ExactString, Conversion[Double, Double]] = Map(
		baseUnit.i -> 1.asScalarFn,
		"km".i -> 1000.asScalarFn,

		// Feet -> Meter
		"ft".i   -> (12/39.370).asScalarFn,

		// Inch -> Meter
		"in".i   -> (1/39.370).asScalarFn,

		// Mile -> Meter
		"mil".e  -> 1609.344.asScalarFn
	)

	override protected def getTSUnit(str: String): TSUnit = {
		new LengthTSUnit(str)
	}

	override val parseMap: Map[ExactString, String] = generateParseMap(Map(
		List("meter".w) -> "m",
		List("kilometer".w) -> "km",
		List("feet".i, "foot".i, "'".e) -> "ft",
		List("inch".w, "\"".e) -> "in",
		List("mile".w) -> "mil"
	))
}