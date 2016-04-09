package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Lengths.
 */
object LengthHelpers{
	object Meters     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("m" )) }
	object Kilometers 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("km")) }

	object Feet     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("ft" )) }
	object Inches 		{ def apply(value: Double = 1) = TSUnitValuePair(value, new LengthTSUnit("in")) }

	private val baseUnit = "m"

	private val conversionMap: Map[String, Conversion[Double, Double]] = Map(
		baseUnit -> 1.asScalarFn,
		"km" -> 1000.asScalarFn,

		// Feet -> Meter
		"ft"   -> (12/39.370).asScalarFn,

		// Inch -> Meter
		"in"   -> (1/39.370).asScalarFn,

		// Mile -> Meter
		"mil"  -> 1609.344.asScalarFn
	)

	private val compressedParseMap = Map(
		"m".i 	-> List("meter".w),
		"km".i 	-> List("kilometer".w),
		"ft".i 	-> List("feet".i, "foot".i, "'".e),
		"in".i 	-> List("inch".w, "\"".e),
		"mil".i -> List("mile".w)
	)

	private[units]  val data = new ConvertibleTSUnitData(baseUnit, "Length", conversionMap, compressedParseMap)
}

/**
 * A convertible unit that measures the length of something.
 *
 * @example
 * 		new LengthTSUnit("m") would measure meters
 * 		new LengthTSUnit("km") would measure kilometers
  * @param unit the name of the unit
 */
class LengthTSUnit(unit: String = "m") extends ConvertibleTSUnit(unit, LengthHelpers.data){
	override protected def getTSUnit(str: String): TSUnit = {
		new LengthTSUnit(str)
	}
}