package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

/**
  * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Temperatures.
  */
object TemperatureHelpers{
	object Fahrenheit     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("F" )) }
	object Celsius 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("C")) }
	object Kelvin 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("K")) }
	object Millikelvin		 	{ def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("mK")) }
	object Rankine     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("R" )) }

	private val baseUnit = "C".i

	private val compressedParseMap = Map(
		baseUnit 	-> List("Celsius".w, "°C".i),
		"F".i 	-> List("Fahrenheit".w, "°F".i),
		"K".i 	-> List("Kelvin".w),
		"R".i -> List("Rankine".w, "°R".i, "°Ra".i, "Ra".i)
	)

	private val edges = List(
		new ConversionEdge[String, Double, Double](baseUnit.baseString, "F", new Conversion[Double, Double]( (a: Double) => a*1.8 + 32, (b: Double) => (b - 32)/1.8), 0.1),
		new ConversionEdge[String, Double, Double](baseUnit.baseString, "K", new Conversion[Double, Double]( (a: Double) => a + 273.15, (b: Double) => b - 273.15), 0.1),
		new ScalarConversionEdge("K", "R", 1.8, 0.1)
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit,	"Temperature",	compressedParseMap,	edges).createMetricUnits(List("K".i))

}

/**
  * A convertible unit that measures the temperature of something.
  *
  * @example
  * 		new TemperatureTSUnit("C") would measure degrees Celsius
  * 		new TemperatureTSUnit("F") would measure degrees Fahrenheit
  * @param unit the name of the unit
  */
class TemperatureTSUnit(unit: String = "m") extends AStarConvertibleTSUnit(unit, TemperatureHelpers.data){
	override protected def getTSUnit(str: String): TSUnit = {
		new TemperatureTSUnit(str)
	}
}