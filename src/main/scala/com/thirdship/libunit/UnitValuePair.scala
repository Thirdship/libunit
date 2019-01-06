package com.thirdship.libunit

import com.thirdship.libunit.units.ScalarUnit

/**
 * A logical grouping of a BaseUnit and a Value that:
 * <ul>
 *     <li>Provides an interface to preform interesting computations on united values.</li>
 *     <li>Allows unit conversions and simplifications</li>
 * </ul>
 *
 * @example
 * 		1m would be UnitValuePair(1,"m")<br>
 * 		.05 m/s would be UnitValuePair(.05, "m/s")<br>
 * 		* assume that "m", "s", and "m/s" are BaseUnits
  * @see [[com.thirdship.libunit.BaseUnit]]
  * @param value the value to apply to the unit
 * @param unit the dimension of the value
 */
final case class UnitValuePair(private val value: Double, private val unit: BaseUnit) {

	/**
	 * Return the value but always simplified
	 */
	lazy val getValue: Double = getSimplified.value

	/**
	 * Return the unit but always simplified
	 */
	lazy val getUnit: BaseUnit = getSimplified.unit

	/**
	 * Return the unit but simplified, note, the UnitValuePair may not change.
	 */
	lazy val getSimplified: UnitValuePair = this.simplify

	/**
	 * Multiplies this by that
	 *
	 * @example
	 * 		"1 (m)" * "2 (m)" = "2 (m m)"<br>
	 * 		"2 (m)" * "3 (s)" = "6 (m s)"<br>
	 * 		"1 (m)" * "1 (s)".inverse = "1 (m / s)"<br>
	 * 		"1 (m)" * "1 (m)".inverse = "1 (Scalar)"<br>
	 * 		*assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	  * @param that is number that would multiply this
	 * @return this * that
	 */
	// scalastyle:off method.name
	def *(that: UnitValuePair): UnitValuePair = UnitValuePair(value * that.value, unit * that.unit).simplify

	/**
	 * Divides this by that
	 *
	 * @example
	 *		"1 (m)" / "2 (s)" = ".5 (m / s)"<br>
	 *		"2 (m)" / "3 (m)" = ".3333 (Scalar)"<br>
	 *		"1 (m)" / "1 (s)".inverse = "1 (m s)"<br>
	 *		"1 (m)" / "1 (m)".inverse = "1 (m m)"<br>
	 *		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	  * @param that is number that would multiply this
	 * @return this / that
	 */
	def /(that: UnitValuePair): UnitValuePair = UnitValuePair(value / that.value, unit / that.unit).simplify

	/**
	 * Adds this and that
	 *
	 * @note that must be convertible to this
	  * @example
	 * 		"1 (m)" + "2 (m)" = "3 (m)"<br>
	 * 		"2 (m)" + "3 (s)" = UnableToConvertUnitsException as "m" is not convertible to "s"<br>
	 * 		"1 (m)" + "1 (km)" = "1001 (m)"<br>
	 * 		"1 (km)" + "1 (m)" = "1.001 (km)"<br>
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	  * @param that is the number to add to this
	 * @return this + that, in the same units as this
	 * @throws UnableToConvertUnitsException the destination is not convertible from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def +(that: UnitValuePair): UnitValuePair = UnitValuePair(value + that.convertTo(this).value, unit)

	/**
	 * Subtracts that from this
	 *
	 * @note that must be convertible to this
	  * @example
	 * 		"1 (m)" - "2 (m)" = "-1 (m)"<br>
	 * 		"2 (m)" - "3 (s)" = UnableToConvertUnitsException as "m" is not convertible to "s"<br>
	 * 		"1 (m)" - "1 (km)" = "-999 (m)"<br>
	 * 		"1 (km)" - "1 (m)" = "-.999 (km)"<br>
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	  * @param that the UnitValuePair to subtract from this
	 * @return this - that, in the same unit as this
	 * @throws UnableToConvertUnitsException the destination is not convertible from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def -(that: UnitValuePair): UnitValuePair = UnitValuePair(value - that.convertTo(this).value, unit)

	def apply(func: (Double) => Double): UnitValuePair = UnitValuePair(func(value), unit)
	def apply(func: (Double, Double) => Double, x: Double): UnitValuePair = UnitValuePair(func(value, x), unit)

	def +(x: Double): UnitValuePair = apply((a, b) => a + b, x)
	def -(x: Double): UnitValuePair = apply((a, b) => a - b, x)
	def /(x: Double): UnitValuePair = apply((a, b) => a / b, x)
	def *(x: Double): UnitValuePair = apply((a, b) => a * b, x)

	def +(x: Int): UnitValuePair = apply((a, b) => a + b, x.toDouble)
	def -(x: Int): UnitValuePair = apply((a, b) => a - b, x.toDouble)
	def /(x: Int): UnitValuePair = apply((a, b) => a / b, x.toDouble)
	def *(x: Int): UnitValuePair = apply((a, b) => a * b, x.toDouble)
	// scalastyle:on method.name
	/**
	 * Finds the inverse of this
	 *
	 * @example
	 *		"5 (m)".inverse = "1/5 (1 / m)"
	 * 		"2 (m)".inverse.inverse = "2 (m)"
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	  * @return 1/this
	 */
	def inverse: UnitValuePair = UnitValuePair(1 / value, unit.inverse)

	/**
	  * Converts this to match thatUnit
	 *
	 * @example
	 * 		"1 (km)".convertTo("m") = "1000 (m)"
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	 * @param thatUnit the unit to convert this to
	 * @return a new UnitValuePair from this that is converted to match thatUnit
	 * @throws UnableToConvertUnitsException the destination is not convertible from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def convertTo(thatUnit: BaseUnit): UnitValuePair = UnitValuePair(unit.convert(thatUnit, value), thatUnit)

	/**
	 * Converts this to match that's unit
	 *
	 * @note
	 *       the conversion function ignores the value of the that, only the unit of that is considered.
	 * @example
	 * 		"1 (km)".convertTo("2 (m)") = "1000 (m)"
	 * @param that the UnitValuePair to convert to
	 * @return a new UnitValuePair from this that is converted to match that's unit
	 * @throws UnableToConvertUnitsException the destination is not convertible from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def convertTo(that: UnitValuePair): UnitValuePair = convertTo(that.getUnit)

	/**
	  * Checks if the current UnitValuePair is convertible to the unit provided
	 *
	 * @param unit the UnitValuePair to check conversion against
	 * @return if this is convertible to that
	 */
	def isConvertible(unit: BaseUnit): Boolean = unit.isConvertible(unit)

	override def toString: String = value + " " + unit

	override def equals(o: Any): Boolean = o match {
		case uvp: UnitValuePair => value == uvp.value && unit.equals(uvp.unit)
		case _ => false
	}

	override def hashCode(): Int = unit.hashCode * value.hashCode

	/**
	 * @return a new UnitValuePair with a simplified Unit
	 */
	protected def simplify: UnitValuePair = unit match {
		// If the unit is a computable unit, if the unit says it is not simplified, then simplify it.
		case u: CompoundUnit => if (u.scalar.value == 1) this
			else UnitValuePair(value * u.scalar.value, new CompoundUnit(u.numerator, u.denominator))
		// If it as scalar, if it is not as simple as it can be, move that simplify
		case u: ScalarUnit => if (u.value == 1) this else UnitValuePair(value * u.value, new ScalarUnit())
		case _ => this
	}

	/**
	  * @return  a new UnitValuePair from this that is converted to the default units of the stored unit
	  */
	def convertToDefaultUnits(): UnitValuePair = convertTo(unit.defaultUnit())
}
