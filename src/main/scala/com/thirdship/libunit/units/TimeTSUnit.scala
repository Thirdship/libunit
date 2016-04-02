package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

object TimeHelpers{
	object Seconds { def apply(value: Double = 1) = UnitValuePair(value, new TimeTSUnit("s" )) }
	object Minutes { def apply(value: Double = 1) = UnitValuePair(value, new TimeTSUnit("min")) }

	val units: List[ExactString] = List(
		"s".i,
		"min".i,
		"h".i,
		"d".i,
		"y".i,
		"jiffy".i

	)

	val edges: List[ConversionEdge] = List(
//		ConversionEdge(new TimeTSUnit("s"),		new TimeTSUnit("s"),		1,			0),
		ConversionEdge(new TimeTSUnit("min"),	new TimeTSUnit("s"),		60,			0),
		ConversionEdge(new TimeTSUnit("h"),		new TimeTSUnit("min"),		60,			0),
		ConversionEdge(new TimeTSUnit("d"),		new TimeTSUnit("h"),		24,			0),
		ConversionEdge(new TimeTSUnit("y"),		new TimeTSUnit("d"),		365.2422,	1),
		ConversionEdge(new TimeTSUnit("s"),		new TimeTSUnit("jiffy"),	3e-24,		0)
	)

	// Store outside the object, that way state is preserved.
	val  aStar: AStarSolver = AStarSolver(units.map(u => new TimeTSUnit(u.baseString)), edges)
}

class TimeTSUnit(unit: String = "s") extends AStarConvertibleTSUnit("Time", unit, "s"){

	override val aStar: AStarSolver = TimeHelpers.aStar
	override val units: List[ExactString] = TimeHelpers.units

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
