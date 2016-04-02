package com.thirdship.libunit

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.utils.{ExactString, WordString, FuzzyString}
import com.thirdship.libunit.units.LengthTSUnit
import com.thirdship.libunit.TSUnitConversion._

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
  */
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

  /**
    * Creates a function that converts from this to a given AStarConvertibleTSUnit
    * @param unit the unit to convert to
    * @return the function
    */
  private def generateConversionFunction(unit: AStarConvertibleTSUnit): (Double) => Double = (a: Double) => a*aStar(this,unit).factor

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
}
