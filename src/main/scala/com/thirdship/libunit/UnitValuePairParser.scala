package com.thirdship.libunit

import java.util.regex.{Matcher, Pattern}

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

	def apply(str: String): Option[UnitValuePair] = parse(str)

	def parseToDefaultUnit(str: String): Option[UnitValuePair] = {
		val parsed = parse(str)
		if (parsed.isDefined) {
			Option(parsed.get.convertToDefaultUnits())
		} else None
	}

	def parse(str: String): Option[UnitValuePair] = {
		// Trim and uniform text
		val tokens = reg.matcher(str.replaceAll("\\s", " ").trim)

		// Split out number and unit
		val matchResult = if (tokens.matches()) Some(tokens)
		else None

		if (matchResult.isDefined) {
			val scalar = matchResult.get

			// if we don't have a value at all, return nothing
			if (Option(scalar.group("number")).isEmpty) return None

			val unitOption = UnitParser(scalar.group("unit"))

			// if our unit is set and isn't a valid unit, return nothing
			if (Option(scalar.group("unit")).isDefined && unitOption.isEmpty) return None

			if(Option(scalar.group("decimal")).isDefined) {
				// number is a floating point number, so parse as double
				val num = scalar.group("number")
					.replaceAll(",", "")
					.toDouble

				Some(UnitValuePair(num, unitOption.get))
			}
			else {
				parseNonMatch(scalar, unitOption)
			}
		}
		else None
	}

	private def parseNonMatch(scalar: Matcher, unitOption: Option[BaseUnit]): Option[UnitValuePair] = {
		val sign = if (Option(scalar.group("sign")).isDefined) scalar.group("sign")
		else ""
		val value: Option[Double] = if (Option(scalar.group("integer")).isDefined) {
			val num = sign + scalar.group("integer").replaceAll(",", "")
			if (Option(scalar.group("exponent")).isDefined) {
				val bigDecimal = new java.math.BigDecimal(num + "e" + scalar.group("exponent"))
				// try to get an exact value with this exponent,
				// and if it has a decimal then just return the double value
				try {
					Some(bigDecimal.longValueExact())
				} catch {
					case _: ArithmeticException => Some(bigDecimal.doubleValue())
				}
			} else Some(java.lang.Long.parseLong(num))
		} else if (Option(scalar.group("hex")).isDefined) {
			val num = sign + scalar.group("hex").substring(2)
			Some(java.lang.Long.parseLong(num, 16)) // scalastyle:ignore magic.number
		} else if (Option(scalar.group("binary")).isDefined) {
			val num = sign + scalar.group("binary").substring(2)
			Some(java.lang.Long.parseLong(num, 2))
		} else None

		if (value.isDefined) {
			val num = value.get
			Some(UnitValuePair(num, unitOption.get))
		} else None
	}
}
