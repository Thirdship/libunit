package com.thirdship.libunit

import org.scalatest.{FlatSpec, Matchers}

class BaseTSUnitTest extends FlatSpec with Matchers {

	implicit class MeasurableString(str: String) {
		def unit: TSUnit = new BaseTSUnit(str)
	}

	"A BaseTSUnit" should "be equal to each other" in {
		"m".unit should be("m".unit)

	}

}