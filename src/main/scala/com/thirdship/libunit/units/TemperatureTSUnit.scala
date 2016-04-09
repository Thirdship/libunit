package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

object TemperatureHelpers{
	object Fahrenheit { def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("f" )) }
	object Celsius { def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("c")) }
	object Kelvin { def apply(value: Double = 1) = TSUnitValuePair(value, new TemperatureTSUnit("k")) }

	private val baseUnit = "c"

	private val compressedParseMap = Map(
		baseUnit.i 	-> List("celsius".i),
		"f".i -> List("fahrenheit".i),
		"k".i -> List("kelvin".i)
	)

	private val edges = List(
		new ScalarConversionEdge(baseUnit,	baseUnit,	1),
		new ConversionEdge(baseUnit, 		"f",	new Conversion[Double, Double](c=> (c * 9.0)/5.0 + 32, f => (f - 32.0) * (5.0/9.0)), 1),
		new ConversionEdge(baseUnit, 		"k",	new Conversion[Double, Double](k => k + 273.150, c=> c -273.150), 1)
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Temperature", compressedParseMap, edges)
}

class TemperatureTSUnit(unit: String = "c") extends AStarConvertibleTSUnit(unit, TemperatureHelpers.data){

	override protected def getTSUnit(str: String): TSUnit = {
		new TemperatureTSUnit(str)
	}
}
