package com.thirdship.libunit

import com.thirdship.libunit.units.MetricPrefixes
import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.utils.{ExactString, WordString, FuzzyString}

/**
  * Holds the data of a AStarConvertibleTSUnit, used to make sure that there is a single source of information for the system
  *
  * @param baseUnit The unit all ConvertableTSUnits of this name can convert to and from.
  * @param humanReadableName The name of the type unit, rather, what it represents. For example: Length, Time etc...
  */
class AStarConvertibleTSUnitData(val baseUnit: ExactString, val humanReadableName: String,
                                 var compressedParseMap: Map[ExactString, List[FuzzyString]],
                                 var conversionEdges: List[ConversionEdge[String, Double, Double]]){

  /**
    * A map of unitName synonyms and the standard unitName
    */
  lazy val parseMap: Map[ExactString, String] = generateParseMap(compressedParseMap)

  // Store outside the object, that way state is preserved.
  lazy val  aStar: AStarSolver = AStarSolver(parseMap.values.toList, conversionEdges)

  def createMetricUnits(units: List[ExactString]) = {
    var metricUnits: MetricPrefixes = new MetricPrefixes("".e, List.empty[FuzzyString])
    units.foreach(unitSuffix => {
      metricUnits = new MetricPrefixes(unitSuffix, compressedParseMap.apply(unitSuffix))
      compressedParseMap ++= metricUnits.compressedParseMap
      conversionEdges ++= metricUnits.edges
    })
    this
  }

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
abstract class AStarConvertibleTSUnit(val unitName: String, val data: AStarConvertibleTSUnitData) extends TSUnit {

  override def defaultUnit(): TSUnit = getTSUnit(data.baseUnit.baseString)

  override def conversionFunction(unit: TSUnit): (Double) => Double = unit match {
    case u: AStarConvertibleTSUnit => generateConversionFunction(u)
    case u: ComputableTSUnit => u.conversionFunction(this)
    case _ => throw new InvalidConversionState(this, unit)
  }

  override def isConvertible(unit: TSUnit): Boolean = unit match {
    case u: AStarConvertibleTSUnit => unitName.equals(unitName)
    case u: ComputableTSUnit => u.isConvertible(this)
    case _ => false
  }

  /**
    * Creates a function that converts from this to a given AStarConvertibleTSUnit
    *
    * @param unit the unit to convert to
    * @return the function
    */
  private def generateConversionFunction(unit: AStarConvertibleTSUnit): (Double) => Double =
    data.aStar.solve(this.unitName, unit.unitName).conversion.to

  override def toString = unitName

  def equalUnits(unit: TSUnit): Boolean = unit match {
    case u: AStarConvertibleTSUnit => data.humanReadableName.equals(u.data.humanReadableName) && unitName.equals(u.unitName)
    case _ => false
  }

  // Implicits
  /**
    * Adds the ability to convert a double to a scalar conversion
    *
    * @param d the double amount to apply to the conversion
    */
  implicit class scalarDouble(d: Double) {
    def asScalarFn = new ScalarConversion(d)
  }

  /**
    * Adds the ability to convert a integer to a scalar conversion
    *
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

  override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] = {
    val syn = data.parseMap.get(str.i) //TODO case sensitive
    if(syn.isDefined)
      Some(getTSUnit(syn.get))
    else
      None
  }

  override def getUnitName = data.baseUnit.baseString
}
