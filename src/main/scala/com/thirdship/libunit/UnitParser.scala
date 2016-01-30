package com.thirdship.libunit

import java.lang.reflect.Modifier

import com.thirdship.libunit.units.ScalarTSUnit
import com.thirdship.util.NameGenerator
import com.typesafe.scalalogging.Logger
import org.reflections.Reflections
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._

object UnitParser{
	private  val reflections = new Reflections("")
	private val logger = Logger(LoggerFactory.getLogger(this.getClass))

	private lazy val unitParser: UnitParser = init(new UnitParser())

	def apply() = unitParser
	def apply(str: String) = unitParser.parse(str)

	def parse(str: String) = unitParser.parse(str)
	def parseAsList(str: String) = unitParser.parseAsList(str)

	private def init(unitParser: UnitParser): UnitParser = {
		logger.info("Initializing static UnitParser: "+unitParser.name)

		val classes = reflections.getSubTypesOf(classOf[TSUnit])
			.asScala
			.filter(c => !Modifier.isAbstract(c.getModifiers))
			.flatMap(clazz => {
			logger.info("("+unitParser.name+") Found "+clazz)
			try {
				val constructor = clazz.getConstructors.head
				logger.trace("("+unitParser.name+") Attempting to instantiate: "+clazz+" with "+ constructor.getParameters.mkString(", "))
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
				case e: Exception => logger.error("("+unitParser.name+") Unable to instantiate: "+clazz, e); None
			}
		}).toList

		unitParser.registerTSUnit(classes)
	}
}

final class UnitParser {

	lazy val name = NameGenerator()

	private val logger = Logger(LoggerFactory.getLogger(this.getClass))

	private[libunit] var classes = Map.empty[Class[_ <: TSUnit], (String) => Option[_ <: TSUnit]]

	def parse(str: String): Option[_ <: TSUnit] = {
		val s = cleanString(str)
		classes.map(c => c._2.apply(s)).flatMap(tso => tso).headOption
	}

	def parseAsList(str: String): List[_ <: TSUnit] = {
		val s = cleanString(str)
		classes.map(c => c._2.apply(s)).flatMap(tso => tso).toList
	}

	def cleanString(str: String): String = {
		str.replaceAll("""[\s\t\n]+""", " ").trim
	}


	/*
	TSUnit registration
	 */

	def registerTSUnit(tsList: List[TSUnit]): UnitParser = {
		tsList foreach registerTSUnit; this
	}

	def registerTSUnit(ts: TSUnit): UnitParser = {
		logger.info("("+name+") Registering "+ts.getClass.getName+ "to UnitParser.")
		classes += ((ts.getClass, (s: String) => {implicit val currentUnitParser = this; ts.parse(s)})); this
	}

	def unregisterTSUnit(ts: TSUnit): UnitParser = {
		logger.info("("+name+") Unregistered "+ts.getClass.getName+ "from UnitParser.")
		classes -= ts.getClass; this
	}

}
