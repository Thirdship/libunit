package com.thirdship.libunit

import com.thirdship.libunit.units.BinaryPrefixes
import com.thirdship.libunit.units.MetricPrefixes
import com.thirdship.libunit.utils.{ExactString, FuzzyString, WordString}
import com.thirdship.libunit.utils.Helpers._

/**
  * Holds the data of a AStarConvertibleTSUnit, used to make sure that there is a single source of information for the system
  *
  * @param baseUnit The unit all ConvertibleTSUnits of this name can convert to and from.
  * @param humanReadableName The name of the type unit, rather, what it represents. For example: Length, Time etc...
  */
class AStarConvertibleTSUnitData(val baseUnit: ExactString, val humanReadableName: String,
                                 val compressedParseMap: Map[ExactString, List[FuzzyString]],
                                 val conversionEdges: List[ConversionEdge[String, Double, Double]]) {

  /**
    * A map of unitName synonyms and the standard unitName
    */
  lazy val parseMap: Map[ExactString, String] = generateParseMap(compressedParseMap)

  // Store outside the object, that way state is preserved.
  lazy val  aStar: AStarSolver = AStarSolver(parseMap.values.toList, conversionEdges)

  def createMetricUnits(units: List[ExactString]): AStarConvertibleTSUnitData = {
	val newStuff = units.map(unitSuffix => {
		new MetricPrefixes(unitSuffix, compressedParseMap.apply(unitSuffix))
	})

	val newCompressedParseMap = newStuff.map(_.compressedParseMap).foldLeft(compressedParseMap)((output, input) => {
		output.++(input)
	})

	val newConversionEdges = newStuff.map(_.edges).foldLeft(conversionEdges)((output, input) => {
		output.++(input)
	})

	new AStarConvertibleTSUnitData(baseUnit, humanReadableName, newCompressedParseMap, newConversionEdges)
  }

  def createBinaryUnits(units: List[ExactString]): AStarConvertibleTSUnitData = {
	  val newStuff = units.map(unitSuffix => {
		  new BinaryPrefixes(unitSuffix, compressedParseMap.apply(unitSuffix))
	  })

	  val newCompressedParseMap = newStuff.map(_.compressedParseMap).foldLeft(compressedParseMap)((output, input) => {
		  output.++(input)
	  })

	  val newConversionEdges = newStuff.map(_.edges).foldLeft(conversionEdges)((output, input) => {
		  output.++(input)
	  })

	  new AStarConvertibleTSUnitData(baseUnit, humanReadableName, newCompressedParseMap, newConversionEdges)
  }

  private def generateParseMap(compressedParseMap: Map[ExactString, List[FuzzyString]]): Map[ExactString, String] = {
    val autoGen = compressedParseMap.keys.map(k => (k, k.baseString)).toMap

    val defined = compressedParseMap.map(keyValuePair => (keyValuePair._1.baseString, keyValuePair._2))
        .flatMap(baseStringValuePair => baseStringValuePair._2.flatMap {
          case wordString: WordString => wordString.asExactStringList
          case exactString: ExactString => List(exactString)
        }
        .map(exactString => (exactString, baseStringValuePair._1)))

    autoGen ++: defined
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
    case u: CompoundTSUnit => u.conversionFunction(this)
    case _ => throw new InvalidConversionState(this, unit)
  }

  override def isConvertible(unit: TSUnit): Boolean = unit match {
    case u: AStarConvertibleTSUnit => unitName.equals(unitName)
    case u: CompoundTSUnit => u.isConvertible(this)
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

  override def toString: String = unitName

  def equalUnits(unit: TSUnit): Boolean = unit match {
    case u: AStarConvertibleTSUnit => data.humanReadableName.equals(u.data.humanReadableName) && unitName.equals(u.unitName)
    case _ => false
  }

  override def unitHashCode: Int = unitName.hashCode

  // Implicits
  /**
    * Adds the ability to convert a double to a scalar conversion
    *
    * @param d the double amount to apply to the conversion
    */
  implicit class scalarDouble(d: Double) {
    def asScalarFn: ScalarConversion = new ScalarConversion(d)
  }

  /**
    * Adds the ability to convert a integer to a scalar conversion
    *
    * @param i the integer amount to apply to the conversion
    */
  implicit class scalarInt(i: Int) {
    def asScalarFn: ScalarConversion = new ScalarConversion(i)
  }

  /**
    * @param str the name of the string to use
    * @return a ts unit of this type with this unit name, no need to verify.
    */
  protected def getTSUnit(str: String): TSUnit

  override private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: TSUnit] = {
    val syn = data.parseMap.get(str.i) // TODO MixedString case
    val ant = data.parseMap.get(str.e)
    if(syn.isDefined) {
      Some(getTSUnit(syn.get))
    } else if (ant.isDefined) {
      Some(getTSUnit(ant.get))
    } else {
      None
    }
  }

  override def getUnitName: String = data.baseUnit.baseString
}
