package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit.{TSUnit, UnitValuePair, Conversion, ConvertableTSUnit}

import com.thirdship.libunit.utils.Helpers._

object TimeHelpers{
	object Seconds { def apply(value: Double = 1) = UnitValuePair(value, new TimeTSUnit("s" )) }
	object Minutes { def apply(value: Double = 1) = UnitValuePair(value, new TimeTSUnit("min")) }
}

class TimeTSUnit(unit: String = "s") extends ConvertableTSUnit("Time", unit, "s"){

	override val conversionMap: Map[ExactString, Conversion[Double, Double]] = Map(
		baseUnit.i	-> 1.asScalarFn,
		"min".i    	-> 60.asScalarFn,
		"h".i      	-> (60 * 60).asScalarFn,
		"d".i     	-> (60 * 60 * 24).asScalarFn,
		"y".i      	-> (60 * 60 * 24 * 365.2422).asScalarFn,
		"jiffy".i 	-> 3e-24.asScalarFn
	)

	override protected def getTSUnit(str: String): TSUnit = {
		new TimeTSUnit(str)
	}

	override val parseMap: Map[ExactString, String] = generateParseMap(Map(
		List("sec".i, "second".w) 	-> "s",
		List("minute".w) 			-> "min",
		List("hr".w, "hour".w) 		-> "h",
		List("day".w) 				-> "d",
		List("yr".w, "year".w) 		-> "y"
	))
}
