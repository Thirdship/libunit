package com.thirdship.libunit

/**
 * Represents a Unit
 *
 * @example
 * 		In the statement 10 meters, meters is the unit and would need to be represented as a BaseUnit
 *
 * @note
 * 		m/s is a unit. However, m/s is a combination of both meter and second units and will be represented as such.
 */
trait BaseUnit {

	/**
	 *	A string that describes what the unit is representing.
	 *
	 * 	@example
	 * 		both Meters and Kilometers are "LengthMeasure#m"
	 */
	final private[libunit] lazy val unitTag = getClass.getSimpleName + "#" + getUnitName

	/**
	 * If convertible, it creates a conversion function to convert this to that, then applies the requested value
	 *
	 * @param that the destination BaseUnit to convert to
	 * @param value the value to convert
	 * @return this->that(value)
	 * @throws UnableToConvertUnitsException the destination is not convertible from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	final def convert(that: BaseUnit, value: Double): Double = {
		val convertFunc = convert(that)
		convertFunc(value)
	}

	/**
	 * If convertible, it creates a conversion function to convert this to that
	 *
	 * @see com.thirdship.libunit.BaseUnit#conversionFunction
	 * @param that the destination BaseUnit to convert to
	 * @return a function to convert this to that
	 * @throws UnableToConvertUnitsException the destination is not convertible from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	final def convert(that: BaseUnit): (Double) => Double = if (isConvertible(that)) {
		// Ask implementations for the conversion function.
		conversionFunction(that)
	} else {
		// We cannot convert, so exit early.
		throw new UnableToConvertUnitsException(this, that)
	}

	/**
	 * Creates a optimized conversion function from one type to another.
	 *
	 * @example
	 * 		"meters".conversionFunction("kilometers") should return (a: Double) => a/1000
	 * 		* Assume that "meters" and "kilometers" are units that are properly defined
	 * @note
	 * 		This code will only be called when this.isConvertible(unit) == true
	 *  	Make sure all function are pure and free of side effect.
	 * @param unit the unit to convert to.
	 * @return a function that maps this.unit to unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	protected def conversionFunction(unit: BaseUnit): (Double) => Double

	/**
	 * Checks to see of the current unit is convertible into the requested unit
	 *
	 * @example
	 * 		"meters".isConvertible("kilometers") == true
	 * 		"seconds".isConvertible("minutes")   == true
	 * 		"meters".isConvertible("minutes")    == false
	 * 		* Assume that "meters", "kilometers", "seconds", and "minutes" are units that are properly defined
	 * @note This should not throw any exceptions, and should consist of only pure methods.
	 * @param unit the unit we want to check conversion into
	 * @return true if this can convert to the specified unit. False otherwise.
	 */
	def isConvertible(unit: BaseUnit): Boolean

	/**
	 *	Raises a unit to a power
	 *
	 * 	Note: This actually is just multiplying the unit n-1 times by its self.
	 *
	 * @param power the power to raise the unit to.
	 * @return the unit raised to that power
	 */
	// scalastyle:off method.name
	def ^(power: Integer): BaseUnit = if (power <= 1) this else this * ^(power-1)

	/**
	 * Multiplies this by that
	 *
	 * @example
	 * 		m * s = m s
	 * 		m * m = m m
	 * 		m * m.inverse = Scalar
	 * 		m * s.inverse = m / s
	 * 		* Assume that m, s, and Scalar are BaseUnits.
	 * 		* Please note that the right hand column is a visual representation on what the BaseUnit would represent
	 * @param that the unit to multiply this by
	 * @return the product of this and that, returns a new BaseUnit
	 */
	def *(that: BaseUnit): BaseUnit = new CompoundUnit(List(this, that), List.empty[BaseUnit]).simplifyType

	/**
	 * Divides this by that
	 *
	 * @example
	 *  	m / s = m / s
	 * 		m / m = Scalar
	 * 		m / m.inverse = m m
	 * 		m / s.inverse = m s
	 * 		* Assume that m, s, and Scalar are BaseUnits.
	 * 		* Please note that the right hand column is a visual representation on what the BaseUnit would represent
	 * @param that the unit to divide this by
	 * @return this / that as a new BaseUnit
	 */
	def /(that: BaseUnit): BaseUnit = new CompoundUnit(List(this), List(that)).simplifyType

	/**
	 * Finds the inverse of this
	 *
	 * @example
	 *		m.inverse = 1 / m
	 * 		m.inverse.inverse = m
	 * 		(m / s).inverse = s / m
	 * 		* Assume that m and s are BaseUnits.
	 * @return 1/this
	 */
	// scalastyle:off method.name
	def inverse: BaseUnit = new CompoundUnit(List.empty[BaseUnit], List(this)).simplifyType

	/**
	 * Checks to see if the unit(s) of one BaseUnit are equals to the unit(s) of another
	 *
	 * @note this function should consist only of pure method calls
	 * @note
	 * 		This is used as a more specific and specialized equality check, used in the equals method.
	 * @example
	 * 		"meter".equalUnits("meters") == true
	 * 		"meter".equalUnits("kilometers") == false
	 * 		* Assume that meter and kilometers are defined and comparable
	 * @param unit the unit to check against
	 * @return if the unit(s) of this and that are the same
	 */
	private[libunit] def equalUnits(unit: BaseUnit): Boolean

	/**
	  * Provides a hash code for the unit
	  *
	  * @note this function should consist only of pure method calls
	  *
	  * @return the corresponding hash code for the unit
	  */
	private[libunit] def unitHashCode: Int

	/**
	 * @return the string representation of the unit represented
	 * @note
	 *       Some BaseUnits will have different units to represent the same data, but here they should return a common value.
	 * @note
	 *        This is used primarily as a way to determine if units are convertible into each other.
	 * @example
	 *  	Say that I want to represent time. I might choose to work with time in seconds. (This decision is arbitrary)
	 *  	If I define the TimeUnit, it would use "s" as it's unitName, as that is what it refers to.
	 *  	Therefore, when I define minutes, it's unit displayed would be 'm', but the unit name would still be 's'.
	 */
	private[libunit] def getUnitName: String


	override def equals(o: Any): Boolean = o match {
		case u: BaseUnit => equalUnits(u)
		case _ => false
	}

	override def hashCode: Int = unitHashCode

	/**
	 * Attempts to parse the string into a ts unit.
	 *
	 * @note,
	 *  An empty option is expected if an incorrect parse occurs, not an exception.
	 * @note,
	 * 	The input to this function should not contain the number part of a unit value pair.
	 * @example,
	 * {{{
	 *     val lm = new LengthUnit("should not matter")
	 *     parse("Meters") //returns a LengthMeasure of dimension m
	 * }}}
	 * @param str the string to parse
	 * @return an option that may contain a BaseUnit.
	 */
	private[libunit] def parse(str: String)(implicit currentUnitParser: UnitParser = UnitParser()): Option[_ <: BaseUnit]

	/**
	  * The name that would be used to characterize the set of units
	  *
	  * For example for LengthTS units, "m" for List("meter","kilometer", "km", ...)
	  */
	def defaultUnit(): BaseUnit
}

class UnitsException(str: String) extends Exception{
	override def getMessage: String = str
}

class InvalidConversion(str: String) extends UnitsException(str) {}

class UnableToConvertUnitsException(source: BaseUnit, target: BaseUnit) extends InvalidConversion(source + " was unable to be converted into " + target) {}

class InvalidConversionState(source: BaseUnit, target: BaseUnit) extends UnableToConvertUnitsException(source, target) {
	override def getMessage: String = source + " was unable to be converted into " + target + ", but passed convertible check."
}
