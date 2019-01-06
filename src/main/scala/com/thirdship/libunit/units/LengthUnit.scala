package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

/**
 * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Lengths.
 */
object LengthHelpers{
	object Meters     	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new LengthUnit("m")) }
	object Kilometers 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new LengthUnit("km")) }
	object Millimeters 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new LengthUnit("mm")) }
	object Microns		 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new LengthUnit("um")) }

	object Feet     	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new LengthUnit("ft")) }
	object Inches 		{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new LengthUnit("in")) }

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

	val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit,	"Length", compressedParseMap, edges).createMetricUnits(List(baseUnit))

}

/**
 * A convertible unit that measures the length of something.
 *
 * @example
 * 		new LengthUnit("m") would measure meters
 * 		new LengthUnit("km") would measure kilometers
  * @param unit the name of the unit
 */
class LengthUnit(unit: String = "m") extends AStarConvertibleUnit(unit, LengthHelpers.data) {
	override protected def getBaseUnit(str: String): BaseUnit = {
		new LengthUnit(str)
	}
}
