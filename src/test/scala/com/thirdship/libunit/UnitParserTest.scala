package com.thirdship.libunit

import org.scalatest.{Matchers, FlatSpec}

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

		UnitParser("[{(m/s)}]")

		println(List(
			UnitParser("m/s"),
			UnitParser("meter/second"),
			UnitParser("meters/seconds"),
			UnitParser("   m / s "),
			UnitParser("(m/s)"),
			UnitParser("   (   m  )  /  (  s )  "),
			UnitParser("  ( (   m  )  /  (  s ) ) "),
			UnitParser(" ( ( (   m  ) ) / ( (  s ) ) )"),
			UnitParser("( ( (   a / ( b )  ) ) / ((  c/d )) )"),
			UnitParser("m^2"),
			UnitParser("m^2/s"),
			UnitParser("m/s^2"),
			UnitParser("m g"),
			UnitParser("ft lb")
		) mkString "\n")


	}

}
