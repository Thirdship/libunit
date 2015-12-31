package com.thirdship.libunit

/**
 * Represents a Unit
 *
 * @example
 * 		In the statement 10 meters, meters is the unit and would need to be represented as a TSUnit
 *
 * @note
 * 		m/s is a unit. However, m/s is a combination of both meter and second units and will be represented as such.
 */
trait TSUnit {

	/**
	 *	A string that describes what the unit is representing.
	 *
	 * 	@example
	 * 		both Meters and Kilometers are "LengthMeasure#m"
	 */
	final private[libunit] lazy val unitTag = getClass.getSimpleName + "#" + getUnitName

	/**
	 * If convertable, it creates a conversion function to convert this to that, then applies the requested value
	 *
	 * @param that the destination TSUnit to convert to
	 * @param value the value to convert
	 * @return this->that(value)
	 * @throws UnableToConvertUnitsException the destination is not convertable from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	final def convert(that: TSUnit, value: Double): Double =convert(that)(value)

	/**
	 * If convertable, it creates a conversion function to convert this to that
	 *
	 * @see com.thirdship.libunit.TSUnit#conversionFunction
	 *
	 * @param that the destination TSUnit to convert to
	 * @return a function to convert this to that
	 * @throws UnableToConvertUnitsException the destination is not convertable from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	final def convert(that: TSUnit): (Double) => Double =  if(isConvertible(that)) {
		//Ask implementations for the conversion function.
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
	 *
	 * @note
	 * 		This code will only be called when this.isConvertable(unit) == true
	 *  	Make sure all function are pure and free of side effect.
	 *
	 * @param unit the unit to convert to.
	 * @return a function that maps this.unit to unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	protected def conversionFunction(unit: TSUnit): (Double) => Double

	/**
	 * Checks to see of the current unit is convertable into the requested unit
	 *
	 * @example
	 * 		"meters".isConvertible("kilometers") == true
	 * 		"seconds".isConvertible("minutes")   == true
	 * 		"meters".isConvertible("minutes")    == false
	 * 		* Assume that "meters", "kilometers", "seconds", and "minutes" are units that are properly defined
	 *
	 * @note This should not throw any exceptions, and should consist of only pure methods.
	 *
	 * @param unit the unit we want to check conversion into
	 * @return true if this can convert to the specified unit. False otherwise.
	 */
	def isConvertible(unit: TSUnit): Boolean

	/**
	 *	Raises a unit to a power
	 *
	 * 	Note: This actually is just multiplying the unit n-1 times by its self.
	 *
	 * @param power the power to raise the unit to.
	 * @return the unit raised to that power
	 */
	def ^(power: Integer): TSUnit = if(power <= 1) this else this * ^(power-1)

	/**
	 * Multiplies this by that
	 *
	 * @example
	 * 		m * s = m s
	 * 		m * m = m m
	 * 		m * m.inverse = Scalar
	 * 		m * s.inverse = m / s
	 * 		* Assume that m, s, and Scalar are TSUnits.
	 * 		* Please note that the right hand column is a visual representation on what the TSUnit would represent
	 *
	 * @param that the unit to multiply this by
	 * @return the product of this and that, returns a new TSUnit
	 */
	def *(that: TSUnit): TSUnit = new ComputableTSUnit(List(this, that), List.empty[TSUnit]).simplifyType

	/**
	 * Divides this by that
	 *
	 * @example
	 *  	m / s = m / s
	 * 		m / m = Scalar
	 * 		m / m.inverse = m m
	 * 		m / s.inverse = m s
	 * 		* Assume that m, s, and Scalar are TSUnits.
	 * 		* Please note that the right hand column is a visual representation on what the TSUnit would represent
	 *
	 * @param that the unit to devide this by
	 * @return this / that as a new TSUnit
	 */
	def /(that: TSUnit): TSUnit = new ComputableTSUnit(List(this), List(that)).simplifyType

	/**
	 * Finds the inverse of this
	 *
	 * @example
	 *		m.inverse = 1 / m
	 * 		m.inverse.inverse = m
	 * 		(m / s).inverse = s / m
	 * 		* Assume that m and s are TSUnits.
	 *
	 * @return 1/this
	 */
	def inverse: TSUnit = new ComputableTSUnit(List.empty[TSUnit], List(this)).simplifyType

	/**
	 * Checks to see if the unit(s) of one TSUnit are equals to the unit(s) of another
	 *
	 * @note this function should consist only of pure method calls
	 *
	 * @note
	 * 		This is used as a more specific and specialized equality check, used in the equals method.
	 *
	 * @example
	 * 		"meter".equalUnits("meters") == true
	 * 		"meter".equalUnits("kilometers") == false
	 * 		* Assume that meter and kilometers are defined and comparable
	 *
	 * @param unit the unit to check against
	 * @return if the unit(s) of this and that are the same
	 */
	private[libunit] def equalUnits(unit: TSUnit): Boolean

	/**
	 * @return the string representation of the unit represented
	 *
	 * @note
	 *       Some TSUnits will have different units to represent the same data, but here they should return a common value.
	 *
	 * @note
	 *        This is used primarily as a way to determine if units are convertable into each other.
	 *
	 * @example
	 *  	Say that I want to represent time. I might choose to work with time in seconds. (This decision is arbitrary)
	 *  	If I define the TimeTSUnit, it would use "s" as it's unitName, as that is what it refers to.
	 *  	Therefore, when I define minutes, it's unit displayed would be 'm', but the unit name would still be 's'.
	 */
	private[libunit] def getUnitName: String


	override def equals(o: Any) = o match {
		case u: TSUnit => equalUnits(u)
		case _ => false
	}

	/**
	 * Attempts to parse the string into a ts unit.
	 *
	 * @note,
	 *  An empty option is expected if an incorrect parse occurs, not an exception.
	 *
	 * @note,
	 * 	The input to this function should not contain the number part of a unit value pair.
	 *
	 * @example,
	 * {{{
	 *     val lm = new LengthTSUnit("should not matter")
	 *     parse("Meters") //returns a LengthMeasure of dimension m
	 * }}}
	 *
	 * @param str the string to parse
	 * @return an option that may contain a TSUnit.
	 */
	private[libunit] def parse(str: String): Option[_ <: TSUnit]
}

class UnitsException(str: String) extends Exception{
	override def getMessage: String = str
}

class InvalidConversion(str: String) extends UnitsException(str){}

class UnableToConvertUnitsException(source: TSUnit, target: TSUnit) extends InvalidConversion(source+" was unable to be converted into "+target){}

class InvalidConversionState(source: TSUnit, target: TSUnit) extends UnableToConvertUnitsException(source, target){
	override def getMessage: String = source + " was unable to be converted into " + target + " but, passed convertable check."
}
