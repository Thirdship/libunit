package com.thirdship.libunit.utils

/**
  * Created by jacobingalls on 12/27/15.
	*
	* Finds matching pairs of parentheses in strings.
 */
object ParenUtils {
	def findMatchingParen(str: String, first_paren: Int): Option[Int] = {

		var level = 0
		for(i <- first_paren until str.length - 1) {

			if (str.charAt(i).equals('(')) {
				level += 1}
			else if (str.charAt(i).equals(')')) {
				level -= 1
				if (level == 0) {
					return Some(i)}
			}
		}

		None
	}

	def cleanParenTypes(str: String): String = {
		val onlyParens = str.replaceAll("""[\[\{]""", "(").replaceAll("""[\]\}]""", ")")

		onlyParens.replaceAll("""\s*\(\s*""", "(").replaceAll("""\s*\)\s*""", ")")
	}

	def removeDuplicates(str: String): String = {
		val dp_regex = """\(\s*\([^\(\)]*\)\s*\)""".r
		val first = dp_regex.findFirstIn(str.trim)
		if(first.isEmpty) return str

		val trimmed = first.get.replaceFirst("""\(""", "").reverse.replaceFirst("""\)""", "").reverse.trim
		removeDuplicates(str.replaceAllLiterally(first.get, trimmed))
	}
}
