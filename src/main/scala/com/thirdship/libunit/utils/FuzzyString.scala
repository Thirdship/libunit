package com.thirdship.libunit.utils

import scala.language.implicitConversions

import org.atteo.evo.inflector.English

import com.thirdship.libunit.ScalarConversion

object Helpers {
	implicit class ImplicitFuzzyString(s: String) {
		def i: ExactString = exact(true)
		def e: ExactString = exact(false)

		def w: WordString = new WordString(s)

		def exact(ignoreCase: Boolean): ExactString = new ExactString(s, ignoreCase)
	}

	/**
	  * Adds the ability to convert a double to a scalar conversion
	  *
	  * @param d the double amount to apply to the conversion
	  */
	implicit class scalarDouble(d: Double) {
		def asScalarFn: ScalarConversion = new ScalarConversion(d)
	}

	/**
	  * Adds the ability to convert a integer to a scalar conversion
	  *
	  * @param i the integer amount to apply to the conversion
	  */
	implicit class scalarInt(i: Int) {
		def asScalarFn: ScalarConversion = new ScalarConversion(i)
	}
}

class ExactString(val str: String, val ignoreCase: Boolean = true) extends FuzzyString(str) {
	override def checkEquality(str: String): Boolean = if (ignoreCase) {
		str.equalsIgnoreCase(baseString)
	} else {
		str.equals(baseString)
	}

	override def equals(obj: Any): Boolean = super.equals(obj)

	override def hashCode: Int = if (ignoreCase) super.hashCode else baseString.hashCode
}

class WordString(str: String) extends FuzzyString(str) {

	lazy val asSingular = new ExactString(str)
	lazy val asPlural = new ExactString(English.plural(str))

	def asExactStringList: List[ExactString] = {
		val s = asSingular
		val p = asPlural
		List(s, p)			// TODO find a way to determine if s is already plural, or that s == p
	}

	override def checkEquality(str: String): Boolean = asSingular.checkEquality(str) || asPlural.checkEquality(str)
}

abstract class FuzzyString(val baseString: String) extends Comparable[FuzzyString]{

	def checkEquality(str: String): Boolean
	def checkEquality(str: FuzzyString): Boolean = checkEquality(str.baseString)

	override def compareTo(o: FuzzyString): Int = baseString.compareTo(o.baseString)

	override def equals(obj: Any): Boolean = {
		obj match {
			case o: FuzzyString => checkEquality(o)
			case s: String => checkEquality(s)
			case _ => false
		}
	}

	override def hashCode: Int = baseString.toLowerCase.hashCode

}
