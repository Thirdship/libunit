package com.thirdship.libunit.units

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.units.LengthHelpers._
import com.thirdship.libunit.units.ScalarHelpers._
import com.thirdship.libunit.units.TimeHelpers._

class ScalarUnitTest extends FlatSpec with Matchers {

	"A Scalar" should "be able to be added and subtracted" in {
    // scalastyle:off magic.number
		(Scalar(10) + Scalar(10)).getValue should be(20)
		(Scalar(10) - Scalar(10)).getValue should be(0)
	}

	it should "when multiplied it should remain a scalar" in {
		(Scalar(10) * Scalar(10)) should be(Scalar(100))
		(Scalar(10) / Scalar(10)) should be(Scalar(1))
	}

	it should "work with other types" in {
		Scalar(10) * Meters(10) should be(Meters(100))
		Scalar(10) / Meters(10) should be(Meters(1).inverse)
		Meters(10) / Meters(10) should be(Scalar(1))
		Meters(10) / Scalar(10) should be(Meters(1))

		((Scalar(1) / Meters(10)) * Seconds(10)).inverse should be(Meters(1) / Seconds(1))

	}

	it should "Provide a scaling mechanism" in {
		new ScalarUnit(10) * new ScalarUnit(10) should be(new ScalarUnit(100))
		new ScalarUnit(10) / new ScalarUnit(10) should be(new ScalarUnit(1))
		// scalastyle:off println
		println(new ScalarUnit(10))

		println(new ScalarUnit(1).conversionFunction(new ScalarUnit(2))(100))
    // scalastyle:on magic.number println
	}

}
