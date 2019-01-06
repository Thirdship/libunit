package com.thirdship.libunit

import java.lang.reflect.Modifier

import scala.collection.JavaConverters._

import com.typesafe.scalalogging.Logger
import org.reflections.Reflections
import org.slf4j.LoggerFactory

import com.thirdship.libunit.units.ScalarUnit
import com.thirdship.util.NameGenerator

object UnitParser{
	private lazy val logger = Logger(LoggerFactory.getLogger(this.getClass))
	private var unitParser: UnitParser = new UnitParser(List("com.thirdship", ""))

	/**
	  * @return the current unitParser
	  */
	def apply(): UnitParser = unitParser

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
	def apply(str: String): Option[BaseUnit] = unitParser.parse(str)

	def parse(str: String): Option[BaseUnit] = unitParser.parse(str)
	def parseAsList(str: String): List[BaseUnit] = unitParser.parseAsList(str)
}

final class UnitParser {

	lazy val name = NameGenerator()

	private[libunit] var classes = Map.empty[Class[_ <: BaseUnit], String => Option[_ <: BaseUnit]]

	def this(searchPaths: List[String]) = {
		this()
		this.classes = getClassesForSearchPaths(searchPaths)
	}

	private def getClassesForSearchPaths(searchPaths: List[String]): Map[Class[_ <: BaseUnit], String => Option[_ <: BaseUnit]] = {
		val reflections = searchPaths.map(new Reflections(_))

		UnitParser.logger.info("Initializing static UnitParser: " + name)

		val classes = reflections.flatMap(_.getSubTypesOf(classOf[BaseUnit])
			.asScala
			.filter(c => ! Modifier.isAbstract(c.getModifiers)))

		UnitParser.logger.info("(" + name + ") Attempting to instantiate: " + classes)

		val instances = classes.flatMap(clazz => {
			UnitParser.logger.info("(" + name + ") Found " + clazz)
			instantiateUnitClass(clazz)
		})

		UnitParser.logger.info("(" + name + ") Registering: " + instances.map(_.getClass.getCanonicalName))

		instances.map(ts => {
			(ts.getClass, (s: String) => {
				implicit val currentUnitParser = this
				ts.parse(s)
			})
		}).toMap
	}

	private def instantiateUnitClass(clazz: Class[_ <: BaseUnit]): Option[BaseUnit] = {
		try {
			val constructor = clazz.getConstructors.head
			UnitParser.logger.trace("(" + name + ") Attempting to instantiate: " + clazz + " with " + constructor.getParameters.mkString(", "))
			val args: Array[_ <: Object] = constructor.getParameterTypes
				.map(t => {
					if (classOf[String].equals(t)) ""
					else if (classOf[Int].equals(t)) 0
					else if (classOf[Double].equals(t)) 0.0d
					else if (classOf[Float].equals(t)) 0.0f
					else if (classOf[Boolean].equals(t)) false
					else if (classOf[List[_]].equals(t)) List.empty[Object]
					else if (classOf[ScalarUnit].equals(t)) new ScalarUnit()
					else null // scalastyle:ignore null
					// TODO use match and replace null
				}.asInstanceOf[Object])

			Some(constructor.newInstance(args: _*).asInstanceOf[BaseUnit])
		} catch {
			case e: Exception => UnitParser.logger.error("(" + name + ") Unable to instantiate: " + clazz, e); None
		}
	}

	def parse(str: String): Option[_ <: BaseUnit] = {
		parseAsList(str).headOption
	}

	def parseAsList(str: String): List[_ <: BaseUnit] = {
		val s = cleanString(Option(str))
		classes.map(c => c._2.apply(s)).flatMap(tso => {
			if (tso.isEmpty) None
			else if (! tso.get.isInstanceOf[CompoundUnit] && "[0-9]".r.findAllMatchIn(s).nonEmpty) None
			else tso
		}).toList
	}

	def cleanString(strOpt: Option[String]): String = strOpt match {
    case Some(str) => str.replaceAll("""[\s\t\n]+""", " ").trim
    case None => ""
	}

}
