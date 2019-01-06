package com.thirdship.libunit

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.units.{LengthUnit, ScalarUnit}

class UnitValuePairTest extends FlatSpec with Matchers {

	implicit class MeasurableString(str: String) {
		def unit: BaseUnit = new InconvertibleUnit(str)
		def lengthUnit: BaseUnit = new LengthUnit(str)
	}

	"A UnitValuePair" should "provide basic arithmetic operations" in {
		// scalastyle:off magic.number
		val m = UnitValuePair(10, "m".unit)
		val s = UnitValuePair(10, "s".unit)
		// scalastyle:on magic.number
		// scalastyle:off println
		println(m)
		println(m * m)
		println(m / s)
		println((m * m) / s)

		println(m + m)
		println(m - m)
		// scalastyle:on println
		a[UnableToConvertUnitsException] should be thrownBy m + s
	}

	it should "provide arithmetic operations to length units" in {
		// scalastyle:off magic.number
		val m = UnitValuePair(10, "m".lengthUnit)
		val km = UnitValuePair(10, "km".lengthUnit)
		// scalastyle:on magic.number
		// scalastyle:off println
		println("Working with m")
		println(m)
		println(m * m)
		println(m / m)

		println(m + m)
		println(m - m)

		println("Working with km")
		println(km)
		println(km * km)
		println(km / km)

		println(km + km)
		println(km - km)

		println("Working with m and km")
		println(km * m)		// 10km * 10m  =  100 (km * m)
		println(m  * km)	// 10m  * 10km =  100 (m  * km)

		println(km / m)
		println(m  / km)

		println(km + m)
		println(m  + km)
		println(km - m)
		println(m  - km)
		// scalastyle:on println
	}

	it should "work with scalars" in {
		// scalastyle:off magic.number
		val s = UnitValuePair(10, new ScalarUnit(10))
		val s2 = s * s
		s.getValue should be(100)
		s2.getValue should be(100*100)
		// scalastyle:on magic.number
	}


	it should "provide arithmetic operations to computable units" in {
		// scalastyle:off magic.number
		val m = UnitValuePair(10, "m".unit)
		val s = UnitValuePair(10, "s".unit)
		// scalastyle:on magic.number
		val m_per_s = m /s
		// scalastyle:off println
		println(m_per_s * m_per_s)
		println(m_per_s / m_per_s)

		println(m_per_s + m_per_s)
		println(m_per_s - m_per_s)
		// scalastyle:on println
	}

}
