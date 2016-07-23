package com.thirdship.libunit.utils

import com.thirdship.libunit.ScalarConversion
import org.atteo.evo.inflector.English

import scala.language.implicitConversions

object Helpers {
	implicit class ImplicitFuzzyString(s: String){
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
		def asScalarFn = new ScalarConversion(d)
	}

	/**
	  * Adds the ability to convert a integer to a scalar conversion
	  *
	  * @param i the integer amount to apply to the conversion
	  */
	implicit class scalarInt(i: Int) {
		def asScalarFn = new ScalarConversion(i)
	}
}

class MixedString(val strings: List[FuzzyString], val separator:String) extends FuzzyString({
	strings.foldLeft(new StringBuilder){ (builder, fuzzy) =>
		builder.append(fuzzy.toString).append(separator)
	}.toString()

}) {

	//The length of the component fuzzy strings summed together (no separators)
	lazy val componentStringLengths = strings.map(_.length).sum
	//strings.foldLeft(0){(sum, fuzzy) => sum + fuzzy.length}


	def this(strings: List[FuzzyString]) {
		this(strings, "")
	}

	override def length: Int = {
		componentStringLengths
	}

	override def checkEquality(str: String): Boolean = {
		//Note: doesn't take separator characters into account in the MixedString or the string to compare it to.
		var strIndex: Int = 0
		for(componentStr:FuzzyString <- strings) {
			val endIndex = strIndex + componentStr.length
			if (endIndex >= str.length || !componentStr.checkEquality(str.substring(strIndex, endIndex))) {
				return false
			}
			strIndex = endIndex
		}
		strIndex == str.length
	}

	// should compareTo check case? it doesn't in ExactString
//	override def compareTo(o:FuzzyString): Int = {
//		throw new NotImplementedError("Implement me!")
//	}

	//This can be implemented if needed, but I'm not sure if it needs to be overridden
//	override def checkEquality(str:FuzzyString): Boolean = {
//		throw new NotImplementedError("Implement me!")
//	}

}


class ExactString(val str: String, val ignoreCase: Boolean = true) extends FuzzyString(str) {
	override def checkEquality(str: String): Boolean = if(ignoreCase)
		str.equalsIgnoreCase(baseString)
	else
		str.equals(baseString)

	override def hashCode: Int = if(ignoreCase) super.hashCode else baseString.hashCode
}

class WordString(str: String) extends FuzzyString(str) {

	lazy val asSingular = new ExactString(str)
	lazy val asPlural = new ExactString(English.plural(str))

	def asExactStringList = {
		val s = asSingular
		val p = asPlural
		List(s,p)			//TODO find a way to determine if s is already plural, or that s == p
	}

	override def checkEquality(str: String): Boolean = asSingular.checkEquality(str) || asPlural.checkEquality(str)
}

abstract class FuzzyString(val baseString: String) extends Comparable[FuzzyString]{

	def checkEquality(str: String): Boolean
	def checkEquality(str: FuzzyString): Boolean = checkEquality(str.baseString)

	def length: Int = {
		baseString.length()
	}

	override def compareTo(o: FuzzyString): Int = {baseString.compareTo(o.baseString)}

	override def equals(obj: Any): Boolean = {
		obj match {
			case o: FuzzyString => checkEquality(o)
			case s: String => checkEquality(s)
			case _ => false
		}
	}

	override def hashCode: Int = baseString.toLowerCase.hashCode

}
