package com.thirdship.libunit

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.units.LengthHelpers.Meters
import com.thirdship.libunit.units.ScalarUnit
import com.thirdship.libunit.units.ScalarHelpers.Scalar
import com.thirdship.libunit.units.TimeHelpers.Seconds

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
		).foreach(s => UnitValuePairParser(s).get.getUnit.getClass.getName should be("com.thirdship.libunit.units.LengthUnit"))


		List(
			"1 s",
			"1.0 min",
			"1.00 year",
			"01.00 years",
			"001.000 yr",
			"01 y"
		).foreach(s => {
			UnitValuePairParser(s).get.getUnit.getClass.getName should be("com.thirdship.libunit.units.TimeUnit")
			UnitValuePairParser(s).get.getValue should be(1)
		})

		List("1", " 1", "	1	", "1 scalar").foreach(s => {
			UnitValuePairParser(s).get.getUnit.getClass.getName should be("com.thirdship.libunit.units.ScalarUnit")
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
		).foreach(a => { val b = UnitValuePairParser(a); b should be(Some(Meters()/Seconds())) })

		Map(
			" 1 m^2 " -> "1 m*m",
			"1 m^2" -> "1 m m",
			"1 m/s^2" -> "1 m /s s"
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
			"1 ((m)/s)",
			"1 (m/(s))",
			"1 ((m)/(s))"
		).foreach(a => { val b = UnitValuePairParser(a); b should be(Some(Meters()/Seconds())) })

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
			"0b0111010" -> 0x3A.toDouble, // 0b 011 1010 or 0x3A
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

			scalar shouldBe defined
			scalar.get.getValue should be(value)
			scalar.get shouldEqual UnitValuePair(value, new ScalarUnit())
		})

		val testBadStrings = List(
			"",
			".",
			"-",
			"+",
			"- meters"
		)
		testBadStrings.foreach(s => {
			UnitValuePairParser(s) shouldNot be(defined)
		})
	}

	it should "parse to default when asked" in {
		// scalastyle:off magic.number
		UnitValuePairParser.parseToDefaultUnit("1 Kilometer").get should be(Meters(1000))
		UnitValuePairParser.parseToDefaultUnit("1 Kilometer / Second").get should be(Meters(1000) / Seconds())

		UnitValuePairParser.parseToDefaultUnit("1 Kilometer / Seconds").get should be(Meters(1000) / Seconds())
		UnitValuePairParser.parseToDefaultUnit("5 Kilometer / Seconds").get should be(Meters(5000) / Seconds())
		UnitValuePairParser.parseToDefaultUnit("0.5 Kilometer / Seconds").get should be(Meters(500) / Seconds())

		UnitValuePairParser.parseToDefaultUnit("5 Kilometer / Meter").get should be(Scalar(5000))
	}

	it should "parse and simplify complex units" in {
		var first: UnitValuePair = null
		var second: BaseUnit = null

		first = UnitValuePairParser.parse("5 ((feet * seconds) / inches)").get
		second = UnitParser.parse("seconds").get
		first.convertTo(second) should be(Seconds(5 * 12))

		first = UnitValuePairParser.parse("2 (ft in s min kg) / (mile hour)").get
		second = UnitParser("in min kg").get
		val converted = first.convertTo(second)
		converted.getUnit.toString shouldEqual "in kg min"
		converted.getValue shouldEqual 2 * 5.26e-8 +- 1e-8
	}
}
