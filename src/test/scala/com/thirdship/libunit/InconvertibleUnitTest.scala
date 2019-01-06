package com.thirdship.libunit

import org.scalatest.{FlatSpec, Matchers}

class InconvertibleUnitTest extends FlatSpec with Matchers {

	implicit class MeasurableString(str: String) {
		def unit: BaseUnit = new InconvertibleUnit(str)
	}

	"An InconvertibleUnit" should "be equal to each other" in {
		"m".unit should be("m".unit)
	}
}
