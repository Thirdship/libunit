package com.thirdship.libunit

import com.thirdship.libunit.utils.{ExactString, FuzzyString, WordString}
import com.thirdship.libunit.utils.Helpers._

/**
  * Holds the data of a ConvertibleUnit, used to make sure that there is a single source of information for the system
  *
  * @param baseUnit The unit all ConvertibleUnits of this name can convert to and from.
  * @param humanReadableName The name of the type unit, rather, what it represents. For example: Length, Time etc...
  * @param conversionMap The map of unitNames to the conversion from that unitName to the baseUnit.
	*                      IE: List("m" -> new ScalarConversion(1), "km" -> new ScalarConversion(1000))
  * @param compressedParseMap A map of unitName synonyms and the standard unitName
  */
class ConvertibleUnitData(val baseUnit: String, val humanReadableName: String,
							val conversionMap: Map[String, Conversion[Double, Double]],
							private val compressedParseMap: Map[ExactString, List[FuzzyString]]) {

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
 * Provides an interface that would allow a BaseUnit to convert to another BaseUnit without changing classes.
 *
 * The idea behind this is to allow a ridged typing system on what a unit represents, not necessarily the scale. This
 * decision was made as the upkeep behind it would be massive, not necessarily between Meters and KiloMeters, but
 * between MetersOverSeconds, MetersSquaredOverMinutes, ect... Instead, we allow units to be the same type and specify
 * their scaling.
 *
 * @param unitName The name of the unit itself. This would be m, km, s ect.
 */
abstract class ConvertibleUnit(val unitName: String,
								 private val data: ConvertibleUnitData) extends BaseUnit {

	override def defaultUnit(): BaseUnit = getBaseUnit(data.baseUnit)

	override def conversionFunction(unit: BaseUnit): Double => Double = unit match {
		case u: ConvertibleUnit => generateConversionFunction(u)
		case u: CompoundUnit => u.conversionFunction(this)
		case _ => throw new InvalidConversionState(this, unit)
	}

	override def isConvertible(unit: BaseUnit): Boolean = unit match {
		case u: ConvertibleUnit =>
			u.data.humanReadableName.equals(data.humanReadableName) &&
			u.data.parseMap.contains(unitName.i)
		case u: CompoundUnit => u.isConvertible(this)
		case _ => false
	}

	/**
	 * Creates a function that converts from this to a given ConvertibleUnit
	  *
	  * @param unit the unit to convert to
	 * @return the function
	 */
	private def generateConversionFunction(unit: ConvertibleUnit): Double => Double = {

		// this this and unit are the same the return a no op function.
		if (unitName.equals(unit.unitName)) {
			return (a: Double) => a
		}

		val standardizedBaseUnit = data.parseMap(unitName.i)
		val toBaseUnit = data.conversionMap(standardizedBaseUnit).to

		val standardizedFromUnit = data.parseMap(unit.unitName.i)
		val fromBaseUnit = data.conversionMap(standardizedFromUnit).from

		a: Double => fromBaseUnit(toBaseUnit(a))
	}

	override def toString: String = unitName

	override def getUnitName: String = data.baseUnit

	def equalUnits(unit: BaseUnit): Boolean = unit match {
		case u: ConvertibleUnit =>
			data.humanReadableName.equals(u.data.humanReadableName) &&
			unitName.equals(u.unitName)
		case _ => false
	}


	/**
	 * @param str the name of the string to use
	 * @return a ts unit of this type with this unit name, no need to verify.
	 */
	protected def getBaseUnit(str: String): BaseUnit

	override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: BaseUnit] = {
		val syn = data.parseMap.get(str.i) // TODO case sensitive
		if (syn.isDefined) {
			Some(getBaseUnit(syn.get))
		} else None
	}

}
