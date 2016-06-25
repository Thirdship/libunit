package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit._

import com.thirdship.libunit.utils.Helpers._

/**
  * Define an assortment of helpers that can create appropriate UnitValuePairs dealing with Lengths.
  */
object StorageHelpers{
	object Bytes    	{ def apply(value: Double = 1) = TSUnitValuePair(value, new StorageTSUnit("B" )) }
	object Bits     	{ def apply(value: Double = 1) = TSUnitValuePair(value, new StorageTSUnit("b")) }
	object Kilobytes  	{ def apply(value: Double = 1) = TSUnitValuePair(value, new StorageTSUnit("KB")) }
	object Megabytes   	{ def apply(value: Double = 1) = TSUnitValuePair(value, new StorageTSUnit("MB")) }

	private val baseUnit = "B".e

	var compressedParseMap = Map(
		baseUnit 	-> List("byte".w),
		"b".e       -> List("bit".w),
		"nibble".i  -> List("nybble".w, "nyble".w, "half-byte".w)
	)

	var edges = List(
		new ScalarConversionEdge(baseUnit.baseString, "b",	8, 0.1),
		new ScalarConversionEdge(baseUnit.baseString, "nibble", 2, 0.1)
	)

	val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit,	"Storage",	compressedParseMap,	edges).createBinaryUnits(List(baseUnit, "b".e))

}

/**
  * A convertible unit that measures the digital storage of something.
  *
  * @example
  * 		new StorageTSUnit("b") would measure bits
  * 		new StorageTSUnit("B") would measure bytes
  * @param unit the name of the unit
  */
class StorageTSUnit(unit: String = "B") extends AStarConvertibleTSUnit(unit, StorageHelpers.data){
	override protected def getTSUnit(str: String): TSUnit = {
		new StorageTSUnit(str)
	}
}