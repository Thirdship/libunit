package com.thirdship.libunit

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.utils.{ExactString, WordString, FuzzyString}
import com.thirdship.libunit.units.LengthTSUnit

abstract class AStarConvertibleTSUnit(val name: String, val unitName: String) extends TSUnit {

  /**
    * A map of unitName synonyms and the standard unitName
    */
  val parseMap: Map[ExactString, String]

  override def conversionFunction(unit: TSUnit): (Double) => Double = unit match {
    case u: AStarConvertibleTSUnit => generateConversionFunction(u)
    case u: AStarComputableTSUnit => u.conversionFunction(this)
    case _ => throw new InvalidConversionState(this, unit)
  }

  override def isConvertible(unit: TSUnit): Boolean = unit match {
    case u: AStarConvertibleTSUnit => unitName.equals(unitName)
    case u: AStarComputableTSUnit => u.isConvertible(this)
    case _ => false
  }

  private def generateConversionFunction(unit: AStarConvertibleTSUnit): (Double) => Double = {
    val conversion = TSUnitConversion.aStar(this,unit)
    (a: Double) => a*conversion.factor
  }

  override def toString = unitName

  def equalUnits(unit: TSUnit): Boolean = unit match {
    case u: AStarConvertibleTSUnit => name.equals(u.name) && unitName.equals(u.unitName)
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

  override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] = {
    val syn = parseMap.get(str.i) //TODO case sensitive
    if(syn.isDefined)
      Some(getTSUnit(syn.get))
    else
      None
  }

  protected def generateParseMap(map: Map[List[FuzzyString], String]): Map[ExactString, String] = {
    val autoGen = conversionMap.keys.map(k => (k, k.baseString)).toMap //TODO make A* friendly

    val defined = map.flatMap(e => e._1.flatMap {
      case fs: WordString => fs.asExactStringList
      case fs: ExactString => List(fs)
    }.map(fs => (fs, e._2)))

    autoGen.++:(defined)
  }
}
