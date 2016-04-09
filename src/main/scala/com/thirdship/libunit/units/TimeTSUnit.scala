package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

object TimeHelpers{
	object Seconds { def apply(value: Double = 1) = TSUnitValuePair(value, new TimeTSUnit("s" )) }
	object Minutes { def apply(value: Double = 1) = TSUnitValuePair(value, new TimeTSUnit("min")) }

	private val baseUnit = "s"

	private val compressedParseMap = Map(
		baseUnit.i 	-> List("sec".i, "second".w),
		"min".i 	-> List("minute".w),
		"h".i 		-> List("hr".w, "hour".w),
		"d".i 		-> List("day".w),
		"y".i 		-> List("yr".w, "year".w),
		"jiffy".i 	-> List()

	)

	private val edges: List[ConversionEdge] = List(
    	ConversionEdge(baseUnit,	baseUnit,	1,			0),
		ConversionEdge("min", 		baseUnit,	60,			0),
		ConversionEdge("h",	 		"min",		60,			0),
		ConversionEdge("d", 		"h",		24,			0),
		ConversionEdge("y", 		"d",		365.2422,	1),
		ConversionEdge(baseUnit, 	"jiffy",	3e-24,		0)
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Time", compressedParseMap, edges)
}

class TimeTSUnit(unit: String = "s") extends AStarConvertibleTSUnit(unit, TimeHelpers.data){

	override protected def getTSUnit(str: String): TSUnit = {
		new TimeTSUnit(str)
	}
}
