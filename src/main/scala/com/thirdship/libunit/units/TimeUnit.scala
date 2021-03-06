package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

object TimeHelpers{
	object Seconds { def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TimeUnit("s")) }
	object Minutes { def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TimeUnit("min")) }

	private val baseUnit = "s".i

	private val compressedParseMap = Map(
    baseUnit -> List("sec".i, "second".w),
    "min".i -> List("minute".w),
    "h".i -> List("hr".w, "hour".w),
    "d".i -> List("day".w),
		"y".i -> List("yr".w, "year".w),
		"jiffy".i -> List()
	)

	private val edges = List(
		// scalastyle:off magic.number
		new ScalarConversionEdge("min", baseUnit.baseString, 60, 0.1),
		new ScalarConversionEdge("h", "min", 60, 0.1),
		new ScalarConversionEdge("d", "h", 24, 0.1),
		new ScalarConversionEdge("y", "d", 365.2422, 1),
		new ScalarConversionEdge(baseUnit.baseString, "jiffy", 3e-24, 1)
		// scalastyle:on magic.number
	)

	val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit, "Time", compressedParseMap, edges).createMetricUnits(List(baseUnit))
}

class TimeUnit(unit: String = "s") extends AStarConvertibleUnit(unit, TimeHelpers.data) {

	override protected def getBaseUnit(str: String): BaseUnit = {
		new TimeUnit(str)
	}
}
