package com.thirdship.libunit

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.utils.{ExactString, WordString, FuzzyString}
import com.thirdship.libunit.units.LengthTSUnit

/**
  * Holds the data of a ConvertibleTSUnit, used to make sure that there is a single source of information for the system
  *
  * @param baseUnit The unit all ConvertableTSUnits of this name can convert to and from.
  * @param humanReadableName The name of the type unit, rather, what it represents. For example: Length, Time etc...
  * @param conversionMap The map of unitNames to the conversion from that unitName to the baseUnit. IE: List("m" -> new ScalarConversion(1), "km" -> new ScalarConversion(1000))
  * @param compressedParseMap A map of unitName synonyms and the standard unitName
  */
class ConvertibleTSUnitData(val baseUnit: String, val humanReadableName: String,
							val conversionMap: Map[String, Conversion[Double, Double]],
							private val compressedParseMap: Map[ExactString, List[FuzzyString]]){

	val parseMap: Map[ExactString, String] = generateParseMap(compressedParseMap)

	private def generateParseMap(compressedParseMap: Map[ExactString, List[FuzzyString]]): Map[ExactString, String] = {
		val autoGen = compressedParseMap.keys.map(k => (k, k.baseString)).toMap

		val defined = compressedParseMap.map(e => (e._1.baseString, e._2)).flatMap(e => e._2.flatMap {
			case fs: WordString => fs.asExactStringList
			case fs: ExactString => List(fs)
		}.map(fs => (fs, e._1)))

		autoGen.++:(defined)
	}
}

/**
 * Provides an interface that would allow a TSUnit to convert to another TSUnit without changing classes.
 *
 * The idea behind this is to allow a ridged typing system on what a unit represents, not necessarily the scale. This
 * decision was made as the upkeep behind it would be massive, not necessarily between Meters and KiloMeters, but
 * between MetersOverSeconds, MetersSquaredOverMinutes, ect... Instead, we allow units to be the same type and specify
 * their scaling.
 *
 * @param unitName The name of the unit itself. This would be m, km, s ect.
 */
abstract class ConvertibleTSUnit(val unitName: String,
								 private val data: ConvertibleTSUnitData) extends TSUnit {

	override def conversionFunction(unit: TSUnit): (Double) => Double = unit match {
		case u: ConvertibleTSUnit => generateConversionFunction(u)
		case u: ComputableTSUnit => u.conversionFunction(this)
		case _ => throw new InvalidConversionState(this, unit)
	}

	override def isConvertible(unit: TSUnit): Boolean = unit match {
		case u: ConvertibleTSUnit =>
			u.data.humanReadableName.equals(data.humanReadableName) &&
			u.data.parseMap.contains(unitName.i)
		case u: ComputableTSUnit => u.isConvertible(this)
		case _ => false
	}

	/**
	 * Creates a function that converts from this to a given ConvertableTSUnit
	  *
	  * @param unit the unit to convert to
	 * @return the function
	 */
	private def generateConversionFunction(unit: ConvertibleTSUnit): (Double) => Double = {

		// this this and unit are the same the return a no op function.
		if (unitName.equals(unit.unitName))
			return (a: Double) => a

		val standardizedBaseUnit = data.parseMap.get(unitName.i).get
		val toBaseUnit = data.conversionMap.get(standardizedBaseUnit).get.toBaseUnit

		val standardizedFromUnit = data.parseMap.get(unit.unitName.i).get
		val fromBaseUnit = data.conversionMap.get(standardizedFromUnit).get.fromBaseUnit

		(a: Double) => fromBaseUnit(toBaseUnit(a))
	}

	override def toString = unitName

	override def getUnitName = data.baseUnit

	def equalUnits(unit: TSUnit): Boolean = unit match {
		case u: ConvertibleTSUnit =>
			data.humanReadableName.equals(u.data.humanReadableName) &&
			unitName.equals(u.unitName)
		case _ => false
	}


	/**
	 * @param str the name of the string to use
	 * @return a ts unit of this type with this unit name, no need to verify.
	 */
	protected def getTSUnit(str: String): TSUnit

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] = {
		val syn = data.parseMap.get(str.i) //TODO case sensitive
		if(syn.isDefined)
			Some(getTSUnit(syn.get))
		else
			None
	}

}
