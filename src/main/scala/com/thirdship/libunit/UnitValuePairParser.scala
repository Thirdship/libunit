package com.thirdship.libunit

import scala.util.matching.Regex

/**
  * Created by jacobingalls on 1/28/16.
  */
object UnitValuePairParser {
	private final val reg = Pattern.compile("""(?x) # x ignores whitespace and comments
		  |(?<number>                       # the entire number
	      |  (?<sign>[\+\-])?               # the sign of the number
	      |  (?:                            # the absolute value of the number
	      |    (?<hex> 0x[0-9a-fA-F]+ )     # hex form of a number
	      |    | (?<binary>0[bB][01]+)      # binary form of a number
	      |    | (?:                        # standard decimal form of a number
	      |      (?<integer>                # the whole integer part of the number
	      |        (?:
	      |          [0]*                   # leading zeroes
	      |          [1-9]                  # a non-zero
	      |          [0-9]{0,2}             # up to two other numbers in first '000,'
	      |          (?:\,[0-9]{3})+        # more consecutive terms of ',000'
	      |        )
	      |        | [0-9]+                 # or just one big number
	      |      )?
	      |      \.?
	      |      (?<decimal>                # the fractional digits in this floating point number
	      |        (?<=\.)[0-9]+
	      |      )?
	      |      (?:                        # exponent for this base 10 number
	      |        e(?<exponent> [\+\-]?[0-9]+ )
	      |      )?
	      |    )
	      |  )
	      |)
	      |(?:                              # units indicating what the number means
	      |  \s* (?<unit>[a-zA-Z'\/"]+)
	      |)?
	      |""".stripMargin)

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
