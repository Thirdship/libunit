package com.thirdship.libunit

import com.thirdship.libunit.units.LengthHelpers.{Kilometers, Meters}
import com.thirdship.libunit.units.ScalarTSUnit
import com.thirdship.libunit.units.TimeHelpers.{Minutes, Seconds}
import org.scalatest.{Matchers, FlatSpec}

class CompoundTSUnitTest extends FlatSpec with Matchers {

	"A CompoundTSUnitTest" should "Simplify Types" in {
		val a = new BaseTSUnit("a")
		val b = new BaseTSUnit("b")
		val c = new BaseTSUnit("c")

		val abc = (a * b) / c
		println(abc)
		abc * abc.inverse should be(new ScalarTSUnit)

	}

	it should "should convert down" in {
		val a = new BaseTSUnit("a")
		val b = new BaseTSUnit("b")

		val a_1 = new CompoundTSUnit(List(a), List.empty[TSUnit])
		val b_1 = new CompoundTSUnit(List(b), List.empty[TSUnit])

		val aa_a = new CompoundTSUnit(List(a,a), List(a))
		val aab_ab = new CompoundTSUnit(List(a,a,b), List(a,b))

		a should be(a)

		a_1 should be(a)
		a should be(a_1)

		a should not be b
		a should not be a_1.inverse
		a should not be b_1

		a/b_1 should be(a/b)

		aa_a should be(a)
		aab_ab should be(aa_a)
		aab_ab should be(a)
	}

	it should "should be convertable" in {
		val a = new BaseTSUnit("a")
		val b = new BaseTSUnit("b")
		val c = new BaseTSUnit("c")


		(a / b).isConvertible(a /b) should equal(true)
		(b / b).isConvertible(a /b) should not equal true
		(a / c).isConvertible(a /b) should not equal true

		val m = Meters(10)
		val km = Kilometers(.01)
		val s = Seconds(10)

		(m / s) should be(m/s)
		(m / s).isConvertible((m / s).getUnit) should equal(true)

		(m/s).isConvertible(((m*m*s)/(s*m*s)).getUnit) should equal(true)

	}

	it should "contain a scaling mechanism" in {
		val a = Meters(1).getUnit * new ScalarTSUnit(30)
		val b = Seconds(1).getUnit * new ScalarTSUnit(15)
		println(a/b)
		println(a*b)
	}

	it should "convert some simple things" in {
		val m = Meters()
		val min = Minutes()
		val km = Kilometers()
		val s = Seconds()


		val a = min.convertTo(s)
		println(a)
		a.getValue should be(60.0)

		val ks = (m/s).convertTo(km/s)
		println(ks)
		ks.getValue should be(.001)

		val mm = (m/s).convertTo(m/min)
		println(mm)
		mm.getValue should be(60)

		val m2 = m * m
		val km2 = km * km

		val kks = (m2/s).convertTo(km2/s)
		println(kks)
		kks.getValue should be(1e-6)

		val mmm = (m2/s).convertTo(m2/min)
		println(mmm)
		mmm.getValue should be(60)

		val kms = (m2/s).convertTo((m * km) / s)
		println(kms)
		kms.getValue should be(1e-3)

		val kmm = (m2/s).convertTo((m * km) / min)
		println(kmm)
		kmm.getValue should be(60e-3 +- 1e-10)
	}

	it should "understand the concept of default units" in {
		(Kilometers()/Minutes()).convertToDefaultUnits().getValue should be(16.66666666666666 +- 1e-10)
		(Kilometers()/Seconds()).convertToDefaultUnits().getValue should be(1000)
		(Meters()/Seconds()).convertToDefaultUnits().getValue should be(1)
		(Meters()/Minutes()).convertToDefaultUnits().getValue should be(0.01666666666666666 +- 1e-10)
	}
}