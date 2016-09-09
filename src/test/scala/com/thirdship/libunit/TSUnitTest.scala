package com.thirdship.libunit

import org.scalatest.{FlatSpec, Matchers}

class TSUnitTest extends FlatSpec with Matchers {

	implicit class MeasurableString(str: String) {
		def unit: TSUnit = new BaseTSUnit(str)
	}

	"A TSUnit" should "print yo" in {
		val a = "a".unit
		val b = "b".unit
		val c = "c".unit
		// scalastyle:off println
		println(a)

		println(". . . . . . . . . . . .")

		println(a * b)

		println(". . . . . . . . . . . .")

		println((a * b) / (c / (a * c)))

		println(". . . . . . . . . . . .")

		val ab = (a^2)/(b^3)

		println((ab * c)/ab)
		// scalastyle:on println
	}

}