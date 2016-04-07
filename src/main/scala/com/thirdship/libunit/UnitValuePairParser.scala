package com.thirdship.libunit

import java.util.regex.Pattern

import com.thirdship.libunit.units.ScalarTSUnit

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
	      |  \s* (?<unit>.+)
	      |)?
	      |""".stripMargin)

	def apply(str: String) = parse(str)

	def parseToDefaultUnit(str: String): Option[TSUnitValuePair] = {
		val parsed = parse(str)
		if(parsed.isDefined) {
			Option(parsed.get.convertToDefaultUnits())
		} else
			None
	}

	def parse(str: String): Option[TSUnitValuePair] ={
		//Trim and uniform text
		val tokens = reg.matcher(str.replaceAll("\\s"," ").trim)

		//Split out number and unit
		val matchResult =if(tokens.matches())
			Some(tokens)
		else
			None

		if(matchResult.isDefined) {
			val scalar = matchResult.get

			// if we don't have a value at all, return nothing
			if(scalar.group("number") == null)
				return None

			val unitOption = UnitParser(scalar.group("unit"))

			// if our unit is set and isn't a valid unit, return nothing
			if(scalar.group("unit") != null && unitOption.isEmpty)
				return None

			if(scalar.group("decimal") != null) {
				//number is a floating point number, so parse as double
				val num = scalar.group("number")
					.replaceAll(",", "")
					.toDouble

				Some( TSUnitValuePair(num, unitOption.get) )
			}
			else {
				val sign = if(scalar.group("sign") != null)
					scalar.group("sign")
				else
					""
				val value: Option[Double] = if(scalar.group("integer") != null) {
					val num = sign + scalar.group("integer").replaceAll(",", "")
					if(scalar.group("exponent") != null) {
						val bigDecimal = new java.math.BigDecimal(num + "e" + scalar.group("exponent"))
						// try to get an exact value with this exponent,
						// and if it has a decimal then just return the double value
						try {
							Some( bigDecimal.longValueExact() )
						}
						catch {
							case _: ArithmeticException => Some( bigDecimal.doubleValue() )
						}
					}
					else
						Some( java.lang.Long.parseLong(num) )
				}
				else if(scalar.group("hex") != null) {
					val num = sign + scalar.group("hex").substring(2)
					Some( java.lang.Long.parseLong(num, 16) )
				}
				else if(scalar.group("binary") != null) {
					val num = sign + scalar.group("binary").substring(2)
					Some( java.lang.Long.parseLong(num, 2) )
				}
				else
					None

				if(value.isDefined) {
					val num = value.get

					Some( TSUnitValuePair(num, unitOption.get) )
				}
				else
					None
			}
		}
		else
			None
	}
}
