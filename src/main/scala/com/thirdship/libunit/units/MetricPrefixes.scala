package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.{ExactString, FuzzyString}
import com.thirdship.libunit.utils.Helpers._

class MetricPrefixes(unit: ExactString, parseList: List[FuzzyString]) {

  lazy val compressedParseMap: Map[ExactString, List[FuzzyString]] = {
    MetricPrefixes.newUnits.map(prefix => {

      // TODO Use MixedString once ATH-241 is resolved
      val keyUnit = if (prefix._1.ignoreCase && unit.ignoreCase) (prefix._1.baseString + unit.baseString).i else (prefix._1.baseString + unit.baseString).e

      val suffixUnits = parseList.flatMap(suffix => {
        val firstUnit = (prefix._1.baseString + suffix.baseString).w
        val altUnits = prefix._2._2.map(str => (str + suffix.baseString).w)
        altUnits :+ firstUnit
      })
      val baseUnits = prefix._2._2.map(str => (str + unit.baseString).w)
      val valueUnits: List[FuzzyString] = suffixUnits ++ baseUnits

      keyUnit -> valueUnits
    })
  }

  val edges: List[ScalarConversionEdge[String]] =
    MetricPrefixes.newUnits.map(prefix => {
      new ScalarConversionEdge(unit.baseString, prefix._1.baseString + unit.baseString, prefix._2._1, 0.1)
    }).toList
}

object MetricPrefixes {
  val newUnits = Map(
    "Y".i -> (1e-24, List("yotta")),
    "Z".i -> (1e-21, List("zetta")),
    "E".i -> (1e-18, List("exa")),
    "P".e -> (1e-15, List("peta")),
    "T".i -> (1e-12, List("tera")),
    "G".i -> (1e-9, List("giga")),
    "M".e -> (1e-6, List("mega")),
    "k".i -> (1e-3, List("kilo")),
    "h".i -> (1e-2, List("hecto")),
    "da".i -> (1e-1, List("deca")),
    "d".i -> (1e1, List("deci")),
    "c".i -> (1e2, List("centi")),
    "m".e -> (1e3, List("milli")),
    "u".i -> (1e6, List("μ", "micro")), // scalastyle:ignore non.ascii.character.disallowed
    "n".i -> (1e9, List("nano")),
    "p".e -> (1e12, List("pico")),
    "f".i -> (1e15, List("femto")),
    "a".i -> (1e18, List("atto"))
  )


}