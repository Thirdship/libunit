package com.thirdship.libunit.units

import com.thirdship.libunit.utils.{ExactString, FuzzyString, WordString}
import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit._

class BinaryPrefixes(unit: ExactString, parseList: List[FuzzyString]) {

	lazy val compressedParseMap: Map[ExactString, List[FuzzyString]] = {
		BinaryPrefixes.newUnits.map(prefix => {

			// TODO Use MixedString once ATH-241 is resolved
			val keyUnit = if (prefix._1.ignoreCase && unit.ignoreCase) (prefix._1.baseString + unit.baseString).i else (prefix._1.baseString + unit.baseString).e
			//println(keyUnit.baseString)

			val suffixUnits = parseList.flatMap(suffix => {
				val firstUnit = (prefix._1.baseString + suffix.baseString).w
				val altUnits = prefix._2._2.map( str => (str + suffix.baseString).w)
				altUnits :+ firstUnit
			})
			val baseUnits = prefix._2._2.map( str => ( str + unit.baseString).w)
			val valueUnits: List[FuzzyString] = suffixUnits ++ baseUnits
			//println(valueUnits.map(_.baseString))

			keyUnit -> valueUnits
		})
	}

	val edges: List[ScalarConversionEdge[String]] =
		BinaryPrefixes.newUnits.map(prefix => {
			new ScalarConversionEdge(unit.baseString, prefix._1.baseString + unit.baseString, prefix._2._1, 0.1)
		}).toList
}

object BinaryPrefixes {
	val newUnits = Map(
		"Yi".i -> (Math.pow(2,80), List("yobi")),
		"Zi".i -> (Math.pow(2,70), List("zebi")),
		"Ei".i -> (Math.pow(2,60), List("exbi")),
		"Pi".i -> (Math.pow(2,50), List("pebi")),
		"Ti".i -> (Math.pow(2,40), List("tebi")),
		"Gi".i -> (Math.pow(2,30), List("gibi")),
		"Mi".i -> (Math.pow(2,20), List("mebi")),
		"Ki".i -> (Math.pow(2,10), List("kibi"))
	)


}