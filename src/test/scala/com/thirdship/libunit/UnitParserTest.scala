package com.thirdship.libunit

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.units.LengthHelpers.Meters
import com.thirdship.libunit.units.TimeHelpers.Seconds

class UnitParserTest extends FlatSpec with Matchers {

	"A UnitParser" should "have units" in {
		UnitParser().classes.isEmpty should equal(false)

		List(
			"m",
			"meter",
			"meters",
			"km",
			"kilometer",
			"kilometers"
		).foreach(s => UnitParser(s).get.getClass.getName should be("com.thirdship.libunit.units.LengthTSUnit"))


		List(
			"s",
			"min",
			"year",
			"years",
			"yr",
			"y"
		).foreach(s => UnitParser(s).get.getClass.getName should be("com.thirdship.libunit.units.TimeTSUnit"))

		List("", " ", "		", "scalar").foreach(s => UnitParser(s).get.getClass.getName should be("com.thirdship.libunit.units.ScalarTSUnit"))

		List("NOT A UNIT!").foreach(UnitParser(_) should be(None))

	}

	it should "parse complicated units" in {

		// Just meters per second and parens.
		List(
			"[{(m/s)}]",
			"m/s",
			"meter/second",
			"meters/seconds",
			"   m / s ",
			"(m/s)",
			"   (   m  )  /  (  s )  ",
			"  ( (   m  )  /  (  s ) ) ",
			" ( ( (   m  ) ) / ( (  s ) ) )",
			" ( ( (   meter  ) ) / ( (  s ) ) )",
			" ( ( (   m  ) ) / ( (  seconds ) ) )",
			" ( ( meters ) / ( (  seconds ) ) )",
			"((meters)/((seconds)))",
			"m * m * s / s * s * m",
			"(m * m * s)/ ( s * s * m)"
		).foreach(a => { val b = UnitParser(a); b should be(Some((Meters()/Seconds()).getUnit)) })

		Map(
			"m^2 " -> "m*m",
			"m^2" -> "m m",
			"m/s^2" -> "m /s s"
		).foreach(a => {
			val b1 = UnitParser(a._1)
			val b2 = UnitParser(a._2)

			b1 should be(b2)
		})

		//	"m g"
		//	"ft lb"

	}

	it should "handle interesting fractions" in {
		List(
			"((m)/s)",
			"(m/(s))",
			"((m)/(s))"
		).foreach(a => {
			val b = UnitParser(a)
			b should be(Some((Meters()/Seconds()).getUnit))
		})

		//			"m^2/s" -> "(m m)/ s",

	}


	it should "be consistent with scalar strings" in {
		// scalastyle:off println
		val a = UnitParser("m 2")
		println(a)
		println(UnitParser("meter 2"))
		println(UnitParser("meters 2"))
		println(UnitParser("m 21"))
		println(UnitParser("meter 21"))
		println(UnitParser("meters 21"))
		// scalastyle:on println
	}

}
