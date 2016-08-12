package com.thirdship.libunit.units

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.UnitParser
import com.thirdship.libunit.units.MassHelpers._

class MassTSUnitTest extends FlatSpec with Matchers {

	"A MassTSUnit" should "convert stuff" in {
		val kg = Kilograms(1)
		val g = Grams(1)

		kg + g should be(Kilograms(1.001))
		kg - g should be(Kilograms(0.999))
		// scalastyle:off magic.number
		g + kg should be(Grams(1001))
		g - kg should be(Grams(-999))

		Slug(10).convertTo(new MassTSUnit("g")) should be(Grams(145939.0))
		// scalastyle:on magic.number
		g should be(g)
		Slug(1) should be(Slug(1))
		PoundMass(1).getUnit should be(PoundMass(1).getUnit)
	}

	it should "be able to convert from a string outside metric units" in {
		UnitParser("lbm").get should be(PoundMass().getUnit)
		UnitParser("pound-mass").get should be(PoundMass().getUnit)
	}

	it should "be able to convert form a string inside metric units" in {
		UnitParser("kilograms").get should be(Kilograms().getUnit)
		UnitParser("kilogs").get should be(Kilograms().getUnit)
		UnitParser("kgrams").get should be(Kilograms().getUnit)
		UnitParser("kg").get should be(Kilograms().getUnit)
		// TODO edge case "kgs"
		// UnitParser("kgs").get should be(Kilograms().getUnit)
	}

	it should "have a default unit" in {
		Grams().getUnit.defaultUnit() === Grams().getUnit
		Kilograms().getUnit.defaultUnit() === Grams().getUnit
		Kilograms().getUnit.defaultUnit() === Kilograms().getUnit
	}

}