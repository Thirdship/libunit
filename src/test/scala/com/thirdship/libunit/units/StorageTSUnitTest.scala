package com.thirdship.libunit.units

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit.UnitParser
import com.thirdship.libunit.units.StorageHelpers._

class StorageTSUnitTest extends FlatSpec with Matchers {

	"A StorageTSUnit" should "convert stuff" in {
		val bit = Bits(1)
		val byte = Bytes(1)
		// scalastyle:off magic.number
		bit + byte should be(Bits(9))
		bit - byte should be(Bits(-7))
		// scalastyle:on magic.number
		(byte + bit) should be(Bytes(1.125))
		(byte - bit) should be(Bytes(0.875))

		bit should be(bit)
		Bits(1) should be(Bits(1))
		Bits(1).getUnit should be(Bits(1).getUnit)
	}

	it should "be able to convert from a string outside binary units" in {
		UnitParser("b").get should be(Bits().getUnit)
		UnitParser("bit").get should be(Bits().getUnit)
		UnitParser("bits") shouldBe defined
		// UnitParser("bits").get should be(Bits().getUnit)
	}

	it should "be able to convert form a string inside binary units" in {
		UnitParser("kilobytes").get should be(Kilobytes().getUnit)
		// TODO Use MixedString
		// UnitParser("kB") shouldBe defined
		// UnitParser("kB").get should be(Kilobytes().getUnit)
		UnitParser("kiloB").get should be(Kilobytes().getUnit)
		UnitParser("Kbyte").get should be(Kilobytes().getUnit)
		UnitParser("KB") shouldBe defined
		// UnitParser("KB").get should be(Kilobytes().getUnit)
	}

	it should "have a default unit" in {
		Bits().getUnit.defaultUnit() ===  Bytes().getUnit
		Bytes().getUnit.defaultUnit() === Bytes().getUnit
	}

}