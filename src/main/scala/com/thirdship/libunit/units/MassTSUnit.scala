package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

/**
  * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Mass.
  */
object MassHelpers{
	object Grams     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new MassTSUnit("g")) }
	object Kilograms 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new MassTSUnit("kg")) }
	object Milligrams 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new MassTSUnit("mg")) }
	object Micrograms	{ def apply(value: Double = 1) = TSUnitValuePair(value, new MassTSUnit("ug")) }

	object Slug     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new MassTSUnit("slug")) }
	object PoundMass 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new MassTSUnit("lbm")) }

	private val baseUnit = "g".i

	private val compressedParseMap = Map(
		baseUnit 	-> List("gram".w),
		"lbm".i 	-> List("pound-mass".w),
		"slug".i    -> List("slug".w)
	)

	private val edges = List(
		new ScalarConversionEdge("slug",	baseUnit.baseString,	14593.9,	1),
		new ScalarConversionEdge("slug",	"lbm",	32.174049,	1)
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit,	"Mass",	compressedParseMap,	edges).createMetricUnits(List(baseUnit))

}

/**
  * A convertible unit that measures the inertial mass of something.
  *
  * @example
  * 		new MassTSUnit("kg") would measure kilograms
  * 		new LengthTSUnit("g") would measure grams
  * @param unit the name of the unit
  */
class MassTSUnit(unit: String = "m") extends AStarConvertibleTSUnit(unit, MassHelpers.data){
	override protected def getTSUnit(str: String): TSUnit = {
		new MassTSUnit(str)
	}
}