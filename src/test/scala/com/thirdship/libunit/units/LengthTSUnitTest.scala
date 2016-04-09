package com.thirdship.libunit.units

import com.thirdship.libunit.{UnitParser, TSUnit}
import com.thirdship.libunit.units.LengthHelpers._
import org.scalatest.{FlatSpec, Matchers}

class LengthTSUnitTest extends FlatSpec with Matchers {

	"A LengthTSUnit" should "convert stuff" in {
		val m = Meters(1)
		val km = Kilometers(1)

		m + km should be(Meters(1001))
		m - km should be(Meters(-999))

		(km + m) should be(Kilometers(1.001))
		(km - m) should be(Kilometers(.999))

		m should be(m)
		Meters(1) should be(Meters(1))
		Meters(1).getUnit should be(Meters(1).getUnit)
	}

	it should "be able to convert from a string" in {
		UnitParser("m").get should be(Meters().getUnit)
		UnitParser("meter").get should be(Meters().getUnit)
		UnitParser("meters").get should be(Meters().getUnit)
	}

	it should "have a default unit" in {
		Meters().getUnit.defaultUnit() ===  Meters().getUnit
		Kilometers().getUnit.defaultUnit() == Meters().getUnit
		Kilometers().getUnit.defaultUnit() == Kilometers().getUnit
	}

}