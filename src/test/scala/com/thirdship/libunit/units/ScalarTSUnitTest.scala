package com.thirdship.libunit.units

import com.thirdship.libunit.units.LengthHelpers._
import com.thirdship.libunit.units.ScalarHelpers._
import com.thirdship.libunit.units.TimeHelpers._

import org.scalatest.{FlatSpec, Matchers}

class ScalarTSUnitTest extends FlatSpec with Matchers {

	"A Scalar" should "be able to be added and subtracted" in {
		(Scalar(10) + Scalar(10)).getValue should be(20)
		(Scalar(10) - Scalar(10)).getValue should be(0)
	}

	it should "when multiplied it should remain a scalar" in {
		(Scalar(10) * Scalar(10)) should be(Scalar(100))
		(Scalar(10) / Scalar(10)) should be(Scalar(1  ))
	}

	it should "work with other types" in {
		Scalar(10) * Meters(10) should be(Meters(100))
		Scalar(10) / Meters(10) should be(Meters(1).inverse)
		Meters(10) / Meters(10) should be(Scalar(1))
		Meters(10) / Scalar(10) should be(Meters(1))

		((Scalar(1) / Meters(10)) * Seconds(10)).inverse should be(Meters(1) / Seconds(1))

	}

	it should "Provide a scaling mechanism" in {
		new ScalarTSUnit(10) * new ScalarTSUnit(10) should be(new ScalarTSUnit(100))
		new ScalarTSUnit(10) / new ScalarTSUnit(10) should be(new ScalarTSUnit(1))

		println(new ScalarTSUnit(10))

		println(new ScalarTSUnit(1).conversionFunction(new ScalarTSUnit(2))(100))
	}

}
