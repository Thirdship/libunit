package com.thirdship.libunit.utils

import com.thirdship.libunit.utils.Helpers._
import org.scalatest.{Matchers, FlatSpec}

class FuzzyStringTest extends FlatSpec with Matchers {

	"An ExactString" should "pass case insensitive equality checks" in {
		val str1 = "hello world".i
		str1.checkEquality("hello world") should equal(true)
		str1.checkEquality("Hello world") should equal(true)
		str1.checkEquality("hello World") should equal(true)
		str1.checkEquality("HEllo WoRlD") should equal(true)
		str1.checkEquality("hello world ") should equal(false)
		str1.checkEquality(" Hello world") should equal(false)
		str1.checkEquality("hello  World") should equal(false)
		str1.checkEquality("HEllo WeoRlD") should equal(false)
		str1.checkEquality("Pizza") should equal(false)

		val str2 = "HeLlO WoRlD".i
		str2.checkEquality("hello world") should equal(true)
		str2.checkEquality("Hello world") should equal(true)
		str2.checkEquality("hello World") should equal(true)
		str2.checkEquality("HEllo WoRlD") should equal(true)

	}

	it should "pass case sensitive equality checks" in {
		val str3 = "hello world".e
		str3.checkEquality("hello world") should equal(true)
		str3.checkEquality("Hello world") should equal(false)
		str3.checkEquality("hello World") should equal(false)
		str3.checkEquality("HEllo WoRlD") should equal(false)

		val str4 = "HeLlO WoRlD".e
		str4.checkEquality("HeLlO WoRlD") should equal(true)
		str4.checkEquality("hello world") should equal(false)
		str4.checkEquality("Hello world") should equal(false)
		str4.checkEquality("hello World") should equal(false)
		str4.checkEquality("HEllo WoRlD") should equal(false)
	}

	it should "more equality tests (case insensitive)" in {
		val i0 = "hello".i
		val i1 = "HELLO".i
		val i2 = "hElLo".i

		val e0 = "hello".e
		val e1 = "HELLO".e
		val e2 = "hElLo".e

		i0.checkEquality("hello") should equal(true)
		i0.checkEquality("HELLO") should equal(true)
		i0.checkEquality("hElLo") should equal(true)

		i1.checkEquality("hello") should equal(true)
		i1.checkEquality("HELLO") should equal(true)
		i1.checkEquality("hElLo") should equal(true)

		i2.checkEquality("hello") should equal(true)
		i2.checkEquality("HELLO") should equal(true)
		i2.checkEquality("hElLo") should equal(true)

		i0.checkEquality(i0) should equal(true)
		i0.checkEquality(i1) should equal(true)
		i0.checkEquality(i2) should equal(true)

		i1.checkEquality(i0) should equal(true)
		i1.checkEquality(i1) should equal(true)
		i1.checkEquality(i2) should equal(true)

		i2.checkEquality(i0) should equal(true)
		i2.checkEquality(i1) should equal(true)
		i2.checkEquality(i2) should equal(true)

		i0.checkEquality(e0) should equal(true)
		i0.checkEquality(e1) should equal(true)
		i0.checkEquality(e2) should equal(true)

		i1.checkEquality(e0) should equal(true)
		i1.checkEquality(e1) should equal(true)
		i1.checkEquality(e2) should equal(true)

		i2.checkEquality(e0) should equal(true)
		i2.checkEquality(e1) should equal(true)
		i2.checkEquality(e2) should equal(true)

	}

	it should "more equality tests (case sensitive)" in {
		val i0 = "hello".i
		val i1 = "HELLO".i
		val i2 = "hElLo".i

		val e0 = "hello".e
		val e1 = "HELLO".e
		val e2 = "hElLo".e

		e0.checkEquality("hello") should equal(true)
		e0.checkEquality("HELLO") should equal(false)
		e0.checkEquality("hElLo") should equal(false)

		e1.checkEquality("hello") should equal(false)
		e1.checkEquality("HELLO") should equal(true)
		e1.checkEquality("hElLo") should equal(false)

		e2.checkEquality("hello") should equal(false)
		e2.checkEquality("HELLO") should equal(false)
		e2.checkEquality("hElLo") should equal(true)

		e0.checkEquality(i0) should equal(true)
		e0.checkEquality(i1) should equal(false)
		e0.checkEquality(i2) should equal(false)

		e1.checkEquality(i0) should equal(false)
		e1.checkEquality(i1) should equal(true)
		e1.checkEquality(i2) should equal(false)

		e2.checkEquality(i0) should equal(false)
		e2.checkEquality(i1) should equal(false)
		e2.checkEquality(i2) should equal(true)

		e0.checkEquality(e0) should equal(true)
		e0.checkEquality(e1) should equal(false)
		e0.checkEquality(e2) should equal(false)

		e1.checkEquality(e0) should equal(false)
		e1.checkEquality(e1) should equal(true)
		e1.checkEquality(e2) should equal(false)

		e2.checkEquality(e0) should equal(false)
		e2.checkEquality(e1) should equal(false)
		e2.checkEquality(e2) should equal(true)

	}

	it should "create hashcode" in {
		val i0 = "hello".i
		val i1 = "HELLO".i
		val i2 = "hElLo".i

		val e0 = "hello".e
		val e1 = "HELLO".e
		val e2 = "hElLo".e

		i0.hashCode should equal(i1.hashCode)
		i1.hashCode should equal(i2.hashCode)

		i0.hashCode should equal(e0.hashCode)

		i0.hashCode should not equal e1.hashCode
		i0.hashCode should not equal e2.hashCode

		e0.hashCode should not equal e1.hashCode
		e0.hashCode should not equal e2.hashCode
		e1.hashCode should not equal e2.hashCode

		"hello".hashCode  should equal(i0.hashCode)
		"hello".hashCode  should equal(i1.hashCode)
		"hello".hashCode  should equal(i2.hashCode)

		"hello".hashCode  should equal(e0.hashCode)
		"HELLO".hashCode  should equal(e1.hashCode)
		"hElLo".hashCode  should equal(e2.hashCode)
	}



	"A WordString" should "provide an interface to a singular and plural string" in {
		val b = "ball".w
		b.checkEquality("ball") should equal(true)
		b.checkEquality("balls") should equal(true)

		b.checkEquality("bAlL") should equal(true)
		b.checkEquality("BaLlS") should equal(true)

		b.checkEquality(" ball") should equal(false)
		b.checkEquality("balls ") should equal(false)
		b.checkEquality("aall") should equal(false)
		b.checkEquality("bzlls") should equal(false)

		val k = "miss".w
		k.checkEquality("miss") should equal(true)
		k.checkEquality("misses") should equal(true)
		k.checkEquality("misss") should equal(false)

		val f = "fry".w
		f.checkEquality("fry") should equal(true)
		f.checkEquality("fries") should equal(true)
		f.checkEquality("frys") should equal(false)
		f.checkEquality("fryes") should equal(false)
		f.checkEquality("frs") should equal(false)
		f.checkEquality("fres") should equal(false)
	}

	it should "make a list of possible equivalents" in {
		"ball".w.asExactStringList should equal(List("ball", "balls"))
		"miss".w.asExactStringList should equal(List("miss", "misses"))
		"fry".w.asExactStringList should equal(List("fry", "fries"))
	}

	it should "produce a reliable hashcode" in {
		"ball".w.hashCode should equal("ball".hashCode)
		"BALL".w.hashCode should equal("ball".hashCode)
		"bAlL".w.hashCode should equal("ball".hashCode)
	}

	"A FuzzyString" should "implement compare to and equals" in {

		Map(
			"a" -> "a",
			"a" -> "b",
			"b" -> "a",
			"a" -> "B",
			"A" -> "B",
			"B" -> "B",
			"B" -> "A",
			"B" -> "a",
			"" -> ""
		).foreach(s => s._1.i.compareTo(s._2.i) should equal(s._1.compareTo(s._2)))

		Map(
			"hello" -> true,
			"hello".i -> true,

			"bye" -> false,
			"bye".i -> false,
			List("Hello") -> false,
			404 -> false
		).foreach(o => "hello".i.equals(o._1) should be(o._2))
	}


}
