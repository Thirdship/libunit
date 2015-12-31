package com.thirdship.libunit

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.utils.{ExactString, WordString, FuzzyString}
import com.thirdship.libunit.units.LengthTSUnit

/**
 * Provides an interface that would allow a TSUnit to convert to another TSUnit without changing classes.
 *
 * The idea behind this is to allow a ridged typing system on what a unit represents, not necessarily the scale. This
 * decision was made as the upkeep behind it would be massive, not necessarily between Meters and KiloMeters, but
 * between MetersOverSeconds, MetersSquaredOverMinutes, ect... Instead, we allow units to be the same type and specify
 * their scaling.
 *
 * @param name The name of the type unit, rather, what it represents. For example: Length, Time etc...
 * @param unitName The name of the unit itself. This would be m, km, s ect.
 * @param baseUnit The unit all ConvertableTSUnits of this name can convert to and from.
 */
abstract class ConvertableTSUnit(val name: String, val unitName: String, val baseUnit: String) extends TSUnit {

	/**
	 * The map of unitNames to the conversion from that unitName to the baseUnit
	 *
	 * For Example,
	 * if we were talking about Meters(m) and KiloMeters(km)
	 * m -> new ScalarConversion(1)
	 * km -> new ScalarConversion(1000)
	 *
	 */
	val conversionMap: Map[ExactString, Conversion[Double, Double]]

	/**
	 * A map of unitName synonyms and the standard unitName
	 */
	val parseMap: Map[ExactString, String]

	override def conversionFunction(unit: TSUnit): (Double) => Double = unit match {
		case u: ConvertableTSUnit => generateConversionFunction(u)
		case u: ComputableTSUnit => u.conversionFunction(this)
		case _ => throw new InvalidConversionState(this, unit)
	}

	override def isConvertible(unit: TSUnit): Boolean = unit match {
		case u: ConvertableTSUnit => name.equals(name) && u.conversionMap.contains(unitName.i)
		case u: ComputableTSUnit => u.isConvertible(this)
		case _ => false
	}

	/**
	 * Creates a function that converts from this to a given ConvertableTSUnit
	 * @param unit the unit to convert to
	 * @return the function
	 */
	private def generateConversionFunction(unit: ConvertableTSUnit): (Double) => Double = {

		// this this and unit are the same the return a no op function.
		if (unitName.equals(unit.unitName))
			return (a: Double) => a

		val toBaseUnit = conversionMap.get(unitName.i).get.toBaseUnit
		val fromBaseUnit = conversionMap.get(unit.unitName.i).get.fromBaseUnit

		(a: Double) => fromBaseUnit(toBaseUnit(a))
	}

	override def toString = unitName

	override def getUnitName = baseUnit

	def equalUnits(unit: TSUnit): Boolean = unit match {
		case u: ConvertableTSUnit => name.equals(u.name) && unitName.equals(u.unitName)
		case _ => false
	}

	// Implicits
	/**
	 * Adds the ability to convert a double to a scalar conversion
	 * @param d the double amount to apply to the conversion
	 */
	implicit class scalarDouble(d: Double) {
		def asScalarFn = new ScalarConversion(d)
	}

	/**
	 * Adds the ability to convert a integer to a scalar conversion
	 * @param i the integer amount to apply to the conversion
	 */
	implicit class scalarInt(i: Int) {
		def asScalarFn = new ScalarConversion(i)
	}

	/**
	 * @param str the name of the string to use
	 * @return a ts unit of this type with this unit name, no need to verify.
	 */
	protected def getTSUnit(str: String): TSUnit

	override private[libunit] def parse(str: String): Option[_ <: TSUnit] = {
		val syn = parseMap.get(str.i) //TODO case sensitive
		if(syn.isDefined)
			Some(getTSUnit(syn.get))
		else
			None
	}

	protected def generateParseMap(map: Map[List[FuzzyString], String]): Map[ExactString, String] = {
		val autoGen = conversionMap.keys.map(k => (k, k.baseString)).toMap

		val defined = map.flatMap(e => e._1.flatMap {
			case fs: WordString => fs.asExactStringList
			case fs: ExactString => List(fs)
		}.map(fs => (fs, e._2)))

		autoGen.++:(defined)
	}
}
