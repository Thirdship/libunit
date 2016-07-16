package com.thirdship.libunit.units

import com.thirdship.libunit.{UnitParser, TSUnit}
import com.thirdship.libunit.units.TemperatureHelpers._
import org.scalatest.{FlatSpec, Matchers}

class TemperatureTSUnitTest extends FlatSpec with Matchers {

	"A TemperatureTSUnit" should "convert stuff" in {
		val c = Celsius(0)
		val f = Fahrenheit(0)
		val k = Kelvin(273.15)
		val r = Rankine(459.67)

		k + c should be(Kelvin(546.3))
		k - c should be(Kelvin(0))

		c + k should be(Celsius(0))
		c - k should be(Celsius(0))

		Celsius(0).convertTo(new TemperatureTSUnit("F")) should be(Fahrenheit(32))
		Celsius(-20).convertTo(new TemperatureTSUnit("F")) should be(Fahrenheit(-4))
		Celsius(-40).convertTo(new TemperatureTSUnit("F")) should be(Fahrenheit(-40))

		(r + f).getValue should equal(919.34 +- 0.001)
		(r - f).getValue should equal(0.0 +- 0.001)
	}

	it should "be able to convert from a string outside metric units" in {
		UnitParser("K").get should be(Kelvin().getUnit)
		UnitParser("celsius").get should be(Celsius().getUnit)
		UnitParser("Ra").get should be(Rankine().getUnit)
	}

	it should "be able to convert form a string inside metric units" in {
		UnitParser("millikelvin").get should be(Millikelvin().getUnit)
		UnitParser("milliK").get should be(Millikelvin().getUnit)
		// TODO ATH-241: MixedString Parser
//		UnitParser("mkelvin").get should be(Millikelvin().getUnit)
//		UnitParser("mK").get should be(Millikelvin().getUnit)
	}

	it should "have a default unit" in {
		Celsius().getUnit.defaultUnit() ===  Celsius().getUnit
		Kelvin().getUnit.defaultUnit() === Celsius().getUnit
		Kelvin().getUnit.defaultUnit() === Kelvin().getUnit
	}

}