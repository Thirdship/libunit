package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Lengths.
 */
object LengthHelpers{
	object Meters     	{ def apply(value: Double = 1): TSUnitValuePair = TSUnitValuePair(value, new LengthTSUnit("m")) }
	object Kilometers 	{ def apply(value: Double = 1): TSUnitValuePair = TSUnitValuePair(value, new LengthTSUnit("km")) }
	object Millimeters 	{ def apply(value: Double = 1): TSUnitValuePair = TSUnitValuePair(value, new LengthTSUnit("mm")) }
	object Microns		 	{ def apply(value: Double = 1): TSUnitValuePair = TSUnitValuePair(value, new LengthTSUnit("um")) }

	object Feet     	{ def apply(value: Double = 1): TSUnitValuePair = TSUnitValuePair(value, new LengthTSUnit("ft")) }
	object Inches 		{ def apply(value: Double = 1): TSUnitValuePair = TSUnitValuePair(value, new LengthTSUnit("in")) }

	private val baseUnit = "m".i

	private val compressedParseMap = Map(
		baseUnit -> List("meter".w),
		"ft".i -> List("feet".i, "foot".i, "'".e),
		"in".i -> List("inch".w, "\"".e),
		"mil".i -> List("mile".w)
	)

	private val edges = List(
    // scalastyle:off magic.number
		new ScalarConversionEdge("in", baseUnit.baseString, 254.0/10000, 1),
		new ScalarConversionEdge("ft", "in", 12, 0.1),
		new ScalarConversionEdge("mil", "ft", 5280, 0.1)
    // scalastyle:on magic.number
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit,	"Length",	compressedParseMap,	edges).createMetricUnits(List(baseUnit))

}

/**
 * A convertible unit that measures the length of something.
 *
 * @example
 * 		new LengthTSUnit("m") would measure meters
 * 		new LengthTSUnit("km") would measure kilometers
  * @param unit the name of the unit
 */
class LengthTSUnit(unit: String = "m") extends AStarConvertibleTSUnit(unit, LengthHelpers.data) {
	override protected def getTSUnit(str: String): TSUnit = {
		new LengthTSUnit(str)
	}
}
