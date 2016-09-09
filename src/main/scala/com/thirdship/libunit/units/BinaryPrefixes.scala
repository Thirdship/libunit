package com.thirdship.libunit.units

import com.thirdship.libunit._
import com.thirdship.libunit.utils.{ExactString, FuzzyString, WordString}
import com.thirdship.libunit.utils.Helpers._

class BinaryPrefixes(unit: ExactString, parseList: List[FuzzyString]) {

	lazy val compressedParseMap: Map[ExactString, List[FuzzyString]] = {
		BinaryPrefixes.newUnits.map(prefix => {

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
		BinaryPrefixes.newUnits.map(prefix => {
			new ScalarConversionEdge(unit.baseString, prefix._1.baseString + unit.baseString, prefix._2._1, 1)
		}).toList
}

object BinaryPrefixes {
	// scalastyle:off magic.number
	val newUnits = Map(
		"Y".i -> (1e-24, List("yotta")),
		"Z".i -> (1e-21, List("zetta")),
		"E".i -> (1e-18, List("exa")),
		"P".e -> (1e-15, List("peta")),
		"T".i -> (1e-12, List("tera")),
		"G".e -> (1e-9, List("giga")),
		"M".e -> (1e-6, List("mega")),
		"K".e -> (1e-3, List("kilo")),
		"Yi".i -> (Math.pow(2, -80), List("yobi")),
		"Zi".i -> (Math.pow(2, -70), List("zebi")),
		"Ei".i -> (Math.pow(2, -60), List("exbi")),
		"Pi".i -> (Math.pow(2, -50), List("pebi")),
		"Ti".i -> (Math.pow(2, -40), List("tebi")),
		"Gi".i -> (Math.pow(2, -30), List("gibi")),
		"Mi".i -> (Math.pow(2, -20), List("mebi")),
		"Ki".i -> (Math.pow(2, -10), List("kibi"))
	)
	// scalastyle:on magic.number


}