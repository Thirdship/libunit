package com.thirdship.libunit

import scala.util.matching.Regex

/**
  * Created by jacobingalls on 1/28/16.
  */
object UnitValuePairParser {
	private final val reg = new Regex("(-*[0-9]*\\.?[0-9]+)\\s*(.*)?", "number", "unit")

	def apply(str: String) = parse(str)

	def parse(str: String): Option[UnitValuePair] ={
		//Trim and uniform text
		val trimmedUnitStr = str.replaceAll("\\s"," ").trim

		//Split out number and unit
		val regOption = reg.findFirstMatchIn(trimmedUnitStr)
		if(regOption.isEmpty){
			return None
		}

		val value = regOption.get.group("number").toDouble
		val unit = UnitParser(regOption.get.group("unit"))

		if(unit.isDefined)
			Some(new UnitValuePair(value, unit.get))
		else
			None
	}
}
