package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

/**
  * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with digital storage units.
  */
object StorageHelpers {
	object Bytes { def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new StorageUnit("B")) }
	object Bits { def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new StorageUnit("b")) }
	object Kilobytes { def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new StorageUnit("KB")) }
	object Megabytes { def apply(value: Double = 1): UnitValuePair = UnitValuePair(value, new StorageUnit("MB")) }

	private val baseUnit = "B".e

	val compressedParseMap = Map(
		baseUnit -> List("byte".w),
		"b".e -> List("bit".w),
		"nibble".i -> List("nybble".w, "nyble".w, "half-byte".w)
	)

	val edges = List(
		// scalastyle:off magic.number
		new ScalarConversionEdge(baseUnit.baseString, "b", 8, 0.1),
		new ScalarConversionEdge(baseUnit.baseString, "nibble", 2, 0.1)
		// scalastyle:off magic.number
	)

	val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit,	"Storage", compressedParseMap, edges)
		.createBinaryUnits(List(baseUnit, "b".e))
}

/**
  * A convertible unit that measures the digital storage of something.
  *
  * @example
  * 		new StorageUnit("b") would measure bits
  * 		new StorageUnit("B") would measure bytes
  * @param unit the name of the unit
  */
class StorageUnit(unit: String = "B") extends AStarConvertibleUnit(unit, StorageHelpers.data) {
	override protected def getBaseUnit(str: String): BaseUnit = {
		new StorageUnit(str)
	}
}
