package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString, WordString}
import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit._

class MetricTSUnit(unit: String, parseList: List[FuzzyString]) {

  lazy val compressedParseMap: Map[ExactString, List[FuzzyString]] = {
    MetricTSUnit.newUnits.map(prefix => {

      // TODO Use MixedString once ATH-241 is resolved
      val keyUnit = if (prefix._1.ignoreCase) (prefix._1.baseString + unit).i else (prefix._1.baseString + unit).e
      //println(keyUnit.baseString)

      val valueUnits = parseList.flatMap(suffix => {
        List((prefix._1.baseString + suffix.baseString).w, (prefix._2._2 + suffix.baseString).w)
      }).::((prefix._2._2 + unit).w)
      //println(valueUnits.map(_.baseString))

      keyUnit -> valueUnits
    })
  }

  val edges: List[ScalarConversionEdge[String]] =
    MetricTSUnit.newUnits.map( prefix => {
      new ScalarConversionEdge(unit, prefix._1.baseString + unit, prefix._2._1, 0.1)
    }).toList
}

object MetricTSUnit {
  val newUnits = Map(
    "E".i -> (1e-18, "exa"),
    "P".e -> (1e-15, "peta"),
    "T".i -> (1e-12, "tera"),
    "G".i -> (1e-9, "giga"),
    "M".e -> (1e-6, "mega"),
    "k".i -> (1e-3, "kilo"),
    "h".i -> (1e-2, "hecto"),
    "da".i -> (1e-1, "deca"),
    "d".i -> (1e1, "deci"),
    "c".i -> (1e2, "centi"),
    "m".e -> (1e3, "milli"),
    "u".i -> (1e6, "micro"),
    "n".i -> (1e9, "nano"),
    "p".e -> (1e12, "pico"),
    "f".i -> (1e15, "femto"),
    "a".i -> (1e18, "atto")
  )


}