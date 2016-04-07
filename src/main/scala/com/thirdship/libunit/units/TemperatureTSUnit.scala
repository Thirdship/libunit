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
		new ConversionEdge("f", 			baseUnit,	new Conversion[Double, Double](f => (f - 32.0) * (5.0/9.0), c=> (c * 9.0)/5.0 + 32), 1),
		new ConversionEdge("k", 			baseUnit,	new Conversion[Double, Double](k => k + 272.150, c=> c -272.150), 1)
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Temperature", compressedParseMap, edges)
}

class TemperatureTSUnit(unit: String = "c") extends AStarConvertibleTSUnit(unit, TemperatureHelpers.data){

	override protected def getTSUnit(str: String): TSUnit = {
		new TemperatureTSUnit(str)
	}
}
