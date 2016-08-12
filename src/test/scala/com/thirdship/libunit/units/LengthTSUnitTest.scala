package com.thirdship.libunit.units

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.UnitParser
import com.thirdship.libunit.units.LengthHelpers._

class LengthTSUnitTest extends FlatSpec with Matchers {

	"A LengthTSUnit" should "convert stuff" in {
		val m = Meters(1)
		val km = Kilometers(1)
		// scalastyle:off magic.number
		m + km should be(Meters(1001))
		m - km should be(Meters(-999))
		// scalastyle:on magic.number
		(km + m) should be(Kilometers(1.001))
		(km - m) should be(Kilometers(.999))

		m should be(m)
		Meters(1) should be(Meters(1))
		Meters(1).getUnit should be(Meters(1).getUnit)
	}

	it should "be able to convert from a string outside metric units" in {
		UnitParser("m").get should be(Meters().getUnit)
		UnitParser("meter").get should be(Meters().getUnit)
		UnitParser("meters").get should be(Meters().getUnit)
	}

	it should "be able to convert form a string inside metric units" in {
		UnitParser("kilometers").get should be(Kilometers().getUnit)
		UnitParser("kiloms").get should be(Kilometers().getUnit)
		UnitParser("kmeters").get should be(Kilometers().getUnit)
		UnitParser("km").get should be(Kilometers().getUnit)
		// TODO edge case "kms"
		// UnitParser("kms").get should be(Kilometers().getUnit)
	}

	it should "have a default unit" in {
		Meters().getUnit.defaultUnit() ===  Meters().getUnit
		Kilometers().getUnit.defaultUnit() == Meters().getUnit
		Kilometers().getUnit.defaultUnit() == Kilometers().getUnit
	}

}