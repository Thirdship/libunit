package com.thirdship.libunit

import com.thirdship.libunit.units.LengthHelpers.Meters
import com.thirdship.libunit.units.ScalarTSUnit
import com.thirdship.libunit.units.TimeHelpers.Seconds
import org.scalatest.{Matchers, FlatSpec}

class UnitValuePairParserTest extends FlatSpec with Matchers {

	"A UnitParser" should "have units" in {
		UnitParser().classes.isEmpty should equal(false)

		List(
			"1 m",
			"1.0 meter",
			"1.00 meters",
			"1 km",
			"1.00 kilometer",
			"1.0000 kilometers"
		).foreach(s => UnitValuePairParser(s).get.getUnit.getClass.getName should be("com.thirdship.libunit.units.LengthTSUnit"))


		List(
			"1 s",
			"1.0 min",
			"1.00 year",
			"01.00 years",
			"001.000 yr",
			"01 y"
		).foreach(s => {
			UnitValuePairParser(s).get.getUnit.getClass.getName should be("com.thirdship.libunit.units.TimeTSUnit")
			UnitValuePairParser(s).get.getValue should be(1)
		})

		List("1", " 1", "	1	", "1 scalar").foreach(s => {
			UnitValuePairParser(s).get.getUnit.getClass.getName should be("com.thirdship.libunit.units.ScalarTSUnit")
			UnitValuePairParser(s).get.getValue should be(1)
		})

		List("NOT A UNIT!").foreach(UnitValuePairParser(_) should be(None))

	}

	it should "parse complicated units" in {

		// Just meters per second and parens.
		List(
			"1 [{(m/s)}]",
			"1 m/s",
			"1 meter/second",
			"1 meters/seconds",
			"1   m / s ",
			"1 (m/s)",
			" 1   (   m  )  /  (  s )  ",
			"  1  ( (   m  )  /  (  s ) ) ",
			"   1.0000 ( ( (   m  ) ) / ( (  s ) ) )",
			" 00001  ( ( (   meter  ) ) / ( (  s ) ) )",
			" 1.0000 ( ( (   m  ) ) / ( (  seconds ) ) )",
			" 1 ( ( meters ) / ( (  seconds ) ) )",
			" 1 ((meters)/((seconds)))",
			" 1 m * m * s / s * s * m",
			"1      (m * m * s)/ ( s * s * m)"
		).foreach(a => { val b = UnitValuePairParser(a); println(a + " --> " + b); b should be(Some(Meters()/Seconds())) })

		Map(
			" 1 m^2 " -> "1 m*m",
			"1 m^2" -> "1 m m",
			"1 m/s^2" -> "1 m /s s"
		).foreach(a => {
			val b1 = UnitParser(a._1)
			val b2 = UnitParser(a._2)

			println(b1 + " ["+a._1+"] should be " + b2 + " ["+a._2+"]")

			b1 should be(b2)
		})

		//	"m g"
		//	"ft lb"

	}

	it should "handle interesting fractions" in {
		List(
			"1 ((m)/s)",
			"1 (m/(s))",
			"1 ((m)/(s))"
		).foreach(a => { val b = UnitValuePairParser(a); println(a + " --> " + b); b should be(Some(Meters()/Seconds())) })

		//			"m^2/s" -> "(m m)/ s",

	}

	it should "check if something is a scalar" in {
		val testGoodStrings: Map[String, Double] = Map(
			"0" -> 0d,
			"1" -> 1d,
			"100" -> 100d,
			"0.5" -> 0.5d,
			"100.5" -> 100.5d,
			".5" -> .5d,
			"1,000,000" -> 1000000d,
			"001,982" -> 1982d,
			"50,001" -> 50001d,
			"-6.4" -> -6.4d,
			"  33.94     " -> 33.94d,
			"0b0111010" -> 0x3A.toDouble, //0b 011 1010 or 0x3A
			"0x0FFF" -> 0x0FFF.toDouble,
			"123e-5" -> 123e-5,
			"123e+5" -> 123e+5,
			"123e5" -> 123e5,
			"125.89e-10" -> 125.89e-10,
			"125.89e+10" -> 125.89e+10
		)
		testGoodStrings.foreach(tuple => {
			val (string, value) = tuple
			val scalar = UnitValuePairParser(string)

			println(s"'$string' should be $value")
			scalar shouldBe defined
			scalar.get.getValue should be(value)
			scalar.get shouldEqual UnitValuePair(value, new ScalarTSUnit())
		})

		val testBadStrings = List(
			"",
			".",
			"-",
			"+",
			"- meters"
		)
		testBadStrings.foreach(s => {
			println(s"'$s' should not be defined")
			UnitValuePairParser(s) shouldNot be(defined)
		})
	}

}
