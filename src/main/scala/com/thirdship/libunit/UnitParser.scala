package com.thirdship.libunit

import java.lang.reflect.Modifier

import com.thirdship.libunit.units.ScalarHelpers.Scalar
import com.thirdship.libunit.units.ScalarTSUnit
import com.thirdship.util.NameGenerator
import com.typesafe.scalalogging.Logger
import org.reflections.Reflections
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._

object UnitParser{
	private lazy val logger = Logger(LoggerFactory.getLogger(this.getClass))
	private var unitParser: UnitParser = new UnitParser(List("com.thirdship",""))

	/**
	  * @return the current unitParser
	  */
	def apply() = unitParser

	/**
	  * Sets the global unit parser to the one passed
 *
	  * @param unitParser the new global unit parser
	  * @return the unit parser passed
	  */
	def apply(unitParser: UnitParser): UnitParser = { this.unitParser = unitParser; this.unitParser }

	/**
	  * Parse the given string
 *
	  * @param str the string to parse for units
	  * @return the Some(unit that was found) or None
	  */
	def apply(str: String) = unitParser.parse(str)

	def parse(str: String) = unitParser.parse(str)
	def parseAsList(str: String) = unitParser.parseAsList(str)
}

final class UnitParser {

	lazy val name = NameGenerator()

	private val logger = Logger(LoggerFactory.getLogger(this.getClass))

	private[libunit] var classes =  Map.empty[Class[_ <: TSUnit], (String) => Option[_ <: TSUnit]]

	def this(searchPaths: List[String]) = {
		this()
		this.classes = getClassesForSearchPaths(searchPaths)
	}

	private def getClassesForSearchPaths(searchPaths: List[String]): Map[Class[_ <: TSUnit], (String) => Option[_ <: TSUnit]] ={
		val reflections = searchPaths.map(new Reflections(_))

		logger.info("Initializing static UnitParser: "+name)

		val classes = reflections.flatMap(_.getSubTypesOf(classOf[TSUnit])
			.asScala
			.filter(c => !Modifier.isAbstract(c.getModifiers)))

		logger.info("("+name+") Attempting to instantiate: "+classes)

		val instances = classes.flatMap(clazz => {
			logger.info("("+name+") Found "+clazz)
			instantiateTSUnitClass(clazz)
		})

		logger.info("("+name+") Registering: "+instances.map(_.getClass.getCanonicalName))

		instances.map(ts => {
			(ts.getClass, (s: String) => {implicit val currentUnitParser = this; ts.parse(s)})
		}).toMap
	}

	private def instantiateTSUnitClass(clazz: Class[_ <: TSUnit]): Option[TSUnit] = {
		try {
			val constructor = clazz.getConstructors.head
			logger.trace("("+name+") Attempting to instantiate: "+clazz+" with "+ constructor.getParameters.mkString(", "))
			val args: Array[_ <: Object] = constructor.getParameterTypes
				.map(t => {
					if(classOf[String].equals(t))
						""
					else if(classOf[Int].equals(t))
						0
					else if(classOf[Double].equals(t))
						0.0d
					else if(classOf[Float].equals(t))
						0.0f
					else if(classOf[Boolean].equals(t))
						false
					else if(classOf[List[_]].equals(t))
						List.empty[Object]
					else if(classOf[ScalarTSUnit].equals(t))
						new ScalarTSUnit()
					else
						null
				}.asInstanceOf[Object])

			Some(constructor.newInstance(args: _*).asInstanceOf[TSUnit])
		} catch {
			case e: Exception => logger.error("("+name+") Unable to instantiate: "+clazz, e); None
		}
	}

	def parse(str: String): Option[_ <: TSUnit] = {
		parseAsList(str).headOption
	}

	def parseAsList(str: String): List[_ <: TSUnit] = {
		val s = cleanString(str)
		classes.map(c => c._2.apply(s)).flatMap(tso => {
			if(tso.isEmpty)
				None
			else if(!tso.get.isInstanceOf[CompoundTSUnit] && "[0-9]".r.findAllMatchIn(s).nonEmpty)
				None
			else
				tso
		}).toList
	}

	def cleanString(str: String): String = {
		if(str == null)
			return ""

		str.replaceAll("""[\s\t\n]+""", " ").trim
	}

}
