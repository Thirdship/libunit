package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

/**
  * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Temperatures.
  */
object TemperatureHelpers{
	object Fahrenheit     	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TemperatureUnit("F")) }
	object Celsius 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TemperatureUnit("C")) }
	object Kelvin 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TemperatureUnit("K")) }
	object Millikelvin		 	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TemperatureUnit("mK")) }
	object Rankine     	{ def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new TemperatureUnit("R")) }

	private val baseUnit = "C".i

	private val compressedParseMap = Map(
		// scalastyle:off non.ascii.character.disallowed
		baseUnit -> List("Celsius".w, "°C".i),
		"F".i -> List("Fahrenheit".w, "°F".i),
		"K".i -> List("Kelvin".w),
		"R".i -> List("Rankine".w, "°R".i, "°Ra".i, "Ra".i)
		// scalastyle:on non.ascii.character.disallowed
	)

	private val edges = List(
		new ConversionEdge[String, Double, Double](baseUnit.baseString, "F", new Conversion[Double, Double](
			(a: Double) => a*1.8 + 32, (b: Double) => (b - 32)/1.8), 0.1),
		new ConversionEdge[String, Double, Double](baseUnit.baseString, "K", new Conversion[Double, Double](
			(a: Double) => a + 273.15, (b: Double) => b - 273.15), 0.1),
		new ScalarConversionEdge("K", "R", 1.8, 0.1)
	)

	val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit,	"Temperature", compressedParseMap, edges).createMetricUnits(List("K".i))

}

/**
  * A convertible unit that measures the temperature of something.
  *
  * @example
  * 		new TemperatureUnit("C") would measure degrees Celsius
  * 		new TemperatureUnit("F") would measure degrees Fahrenheit
  * @param unit the name of the unit
  */
class TemperatureUnit(unit: String = "m") extends AStarConvertibleUnit(unit, TemperatureHelpers.data) {
	override protected def getBaseUnit(str: String): BaseUnit = {
		new TemperatureUnit(str)
	}
}
