package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

/**
  * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Mass.
  */
object MassHelpers{
	object Grams     	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new MassUnit("g")) }
	object Kilograms 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new MassUnit("kg")) }
	object Milligrams 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new MassUnit("mg")) }
	object Micrograms	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new MassUnit("ug")) }

	object Slug     	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new MassUnit("slug")) }
	object PoundMass 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new MassUnit("lbm")) }

	private val baseUnit = "g".i

	private val compressedParseMap = Map(
		baseUnit -> List("gram".w),
		"lbm".i -> List("pound-mass".w),
		"slug".i -> List("slug".w)
	)

	private val edges = List(
		new ScalarConversionEdge("slug",	baseUnit.baseString,	14593.9,	1),
		new ScalarConversionEdge("slug",	"lbm",	32.174049,	1)
	)

	val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit,	"Mass", compressedParseMap, edges).createMetricUnits(List(baseUnit))

}

/**
  * A convertible unit that measures the inertial mass of something.
  *
  * @example
  * 		new MassUnit("kg") would measure kilograms
  * 		new LengthUnit("g") would measure grams
  * @param unit the name of the unit
  */
class MassUnit(unit: String = "m") extends AStarConvertibleUnit(unit, MassHelpers.data) {
	override protected def getBaseUnit(str: String): BaseUnit = {
		new MassUnit(str)
	}
}
