package com.thirdship.libunit

import com.thirdship.libunit.units.{ScalarTSUnit, LengthTSUnit}
import org.scalatest.{Matchers, FlatSpec}

class TSUnitValuePairTest extends FlatSpec with Matchers {

	implicit class MeasurableString(str: String) {
		def unit: TSUnit = new BaseTSUnit(str)
		def lengthUnit: TSUnit = new LengthTSUnit(str)
	}

	"A UnitValuePair" should "provide basic arithmetic operations" in {
		val m = new TSUnitValuePair(10, "m".unit)
		val s = new TSUnitValuePair(10, "s".unit)

		println(m)
		println(m * m)
		println(m / s)
		println((m * m) / s)

		println(m + m)
		println(m - m)

		a [UnableToConvertUnitsException] should be thrownBy m + s
	}

	it should "provide arithmetic operations to length units" in {
		val m  = new TSUnitValuePair(10, "m".lengthUnit)
		val km = new TSUnitValuePair(10, "km".lengthUnit)

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
		println(km * m)		//10km * 10m  =  100 (km * m)
		println(m  * km)	//10m  * 10km =  100 (m  * km)

		println(km / m)
		println(m  / km)

		println(km + m)
		println(m  + km)
		println(km - m)
		println(m  - km)
	}

	it should "work with scalars" in {
		val s = new TSUnitValuePair(10,new ScalarTSUnit(10))
		val s2 =  s * s
		s.getValue should be(100)
		s2.getValue should be(100*100)
	}


	it should "provide arithmetic operations to computable units" in {
		val m = new TSUnitValuePair(10, "m".unit)
		val s = new TSUnitValuePair(10, "s".unit)
		val m_per_s = m /s

		println(m_per_s * m_per_s)
		println(m_per_s / m_per_s)

		println(m_per_s + m_per_s)
		println(m_per_s - m_per_s)

	}

}