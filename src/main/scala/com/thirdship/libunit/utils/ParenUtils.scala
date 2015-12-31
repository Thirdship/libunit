package com.thirdship.libunit.utils

/**
 * Created by jacobingalls on 12/27/15.
 */
object ParenUtils {
	def cleanParenTypes(str: String): String = {
		val onlyParens = str.replaceAll("""[\[\{]""","(").replaceAll("""[\]\}]""",")")

		onlyParens.replaceAll("""\s*\(\s*""","(").replaceAll("""\s*\)\s*""",")")
	}

	def removeDuplicates(str: String): String = {
		val dp_regex = """\(\s*\([^\(\)]*\)\s*\)""".r
		val first = dp_regex.findFirstIn(str.trim)
		if(first.isEmpty) return str

		val trimmed = first.get.replaceFirst("""\(""","").reverse.replaceFirst("""\)""","").reverse.trim
		removeDuplicates(str.replaceAllLiterally(first.get, trimmed))
	}
}
