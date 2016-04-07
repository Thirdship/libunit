package com.thirdship.libunit.units

import com.thirdship.libunit.UnitParser
import com.thirdship.libunit.units.TemperatureHelpers.{Fahrenheit, Kelvin, Celsius}
import org.scalatest.{FlatSpec, Matchers}

class TemperatureTSUnitTest extends FlatSpec with Matchers {

	"A TemperatureTSUnit" should "convert stuff" in {
		val c = Celsius()
		val k = Kelvin()
		val f = Fahrenheit()

		c.convertTo(k.getUnit).getValue === 272.150
		k.convertTo(c.getUnit).getValue === -272.150

		c.convertTo(f.getUnit).getValue === 33.8
		f.convertTo(c.getUnit).getValue === -17.22
	}

	it should "be able to convert from a string" in {
		UnitParser("c").get should be(Celsius().getUnit)
		UnitParser("celsius").get should be(Celsius().getUnit)
		UnitParser("kelvin").get should be(Kelvin().getUnit)
	}

	it should "have a default unit" in {
		Celsius().getUnit.defaultUnit() === Celsius().getUnit
		Kelvin().getUnit.defaultUnit() === Celsius().getUnit
		Fahrenheit().getUnit.defaultUnit() === Celsius().getUnit
	}

}