package com.thirdship.libunit

import com.thirdship.libunit.units.ScalarTSUnit

/**
 * A logical grouping of a TSUnit and a Value that:
 * <ul>
 *     <li>Provides an interface to preform interesting computations on united values.</li>
 *     <li>Allows unit conversions and simplifications</li>
 * </ul>
 *
 * @example
 * 		1m would be UnitValuePair(1,"m")<br>
 * 		.05 m/s would be UnitValuePair(.05, "m/s")<br>
 * 		* assume that "m", "s", and "m/s" are TSUnits
 * @see [[com.thirdship.libunit.TSUnit]]
 * @param value the value to apply to the unit
 * @param unit the dimension of the value
 */
final case class TSUnitValuePair(private val value: Double, private val unit: TSUnit) {

	/**
	 * Return the value but always simplified
	 */
	lazy val getValue: Double = getSimplified.value

	/**
	 * Return the unit but always simplified
	 */
	lazy val getUnit: TSUnit  = getSimplified.unit

	/**
	 * Return the unit but simplified, note, the UnitValuePair may not change.
	 */
	lazy val getSimplified: TSUnitValuePair = this.simplify

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
	def *(that: TSUnitValuePair) = new TSUnitValuePair(value * that.value, unit * that.unit).simplify

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
	def /(that: TSUnitValuePair) = new TSUnitValuePair(value / that.value, unit / that.unit).simplify

	/**
	 * Adds this and that
	 *
	 * @note that must be convertable to this
	 * @example
	 * 		"1 (m)" + "2 (m)" = "3 (m)"<br>
	 * 		"2 (m)" + "3 (s)" = UnableToConvertUnitsException as "m" is not convertable to "s"<br>
	 * 		"1 (m)" + "1 (km)" = "1001 (m)"<br>
	 * 		"1 (km)" + "1 (m)" = "1.001 (km)"<br>
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	 * @param that is the number to add to this
	 * @return this + that, in the same units as this
	 * @throws UnableToConvertUnitsException the destination is not convertable from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def +(that: TSUnitValuePair) = new TSUnitValuePair(value + that.convertTo(this).value, unit)

	/**
	 * Subtracts that from this
	 *
	 * @note that must be convertable to this
	 * @example
	 * 		"1 (m)" - "2 (m)" = "-1 (m)"<br>
	 * 		"2 (m)" - "3 (s)" = UnableToConvertUnitsException as "m" is not convertable to "s"<br>
	 * 		"1 (m)" - "1 (km)" = "-999 (m)"<br>
	 * 		"1 (km)" - "1 (m)" = "-.999 (km)"<br>
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	 * @param that the UnitValuePair to subtract from this
	 * @return this - that, in the same unit as this
	 * @throws UnableToConvertUnitsException the destination is not convertable from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def -(that: TSUnitValuePair) = new TSUnitValuePair(value - that.convertTo(this).value, unit)

	/**
	 * Finds the inverse of this
	 *
	 * @example
	 *		"5 (m)".inverse = "1/5 (1 / m)"
	 * 		"2 (m)".inverse.inverse = "2 (m)"
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	 * @return 1/this
	 */
	def inverse = new TSUnitValuePair(1/value, unit.inverse)

	/**
	  * Converts this to match thatUnit
	 *
	 * @example
	 * 		"1 (km)".convertTo("m") = "1000 (m)"
	 * 		* assume correct parsing of data into UnitValuePairs where "Value (Unit)"
	 * @param thatUnit the unit to convert this to
	 * @return this converted to match thatUnit
	 * @throws UnableToConvertUnitsException the destination is not convertable from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def convertTo(thatUnit: TSUnit): TSUnitValuePair = new TSUnitValuePair(unit.convert(thatUnit, value), thatUnit)

	/**
	 * Converts this to match that's unit
	 *
	 * @note
	 *       the conversion function ignores the value of the that, only the unit of that is considered.
	 * @example
	 * 		"1 (km)".convertTo("2 (m)") = "1000 (m)"
	 * @param that the UnitValuePair to convert to
	 * @return this converted to match that's unit
	 * @throws UnableToConvertUnitsException the destination is not convertable from the current unit
	 * @throws UnitsException when unit conversion occurs but there is an error
	 */
	def convertTo(that: TSUnitValuePair): TSUnitValuePair = convertTo(that.unit)

	/**
	  * Checks if the current UnitValuePair is convertable to the unit provided
	 *
	 * @param unit the UnitValuePair to check conversion against
	 * @return if this is convertable to that
	 */
	def isConvertible(unit: TSUnit): Boolean = unit.isConvertible(unit)

	override def toString = value+ " " +unit

	override def equals(o: Any) = o match {
		case uvp: TSUnitValuePair => value == uvp.value && unit.equals(uvp.unit)
		case _ => false
	}

	/**
	 * @return a new UnitValuePair with a simplified Unit
	 */
	 protected def simplify:TSUnitValuePair = unit match {
		// If the unit is a computable unit, if the unit says it is not simplified, then simplify it.
		case u: ComputableTSUnit => if(u.scalar.value == 1)
			this else new TSUnitValuePair(value * u.scalar.value, new ComputableTSUnit(u.numerator, u.denominator))
		// If it as scalar, if it is not as simple as it can be, move that simplify
		case u: ScalarTSUnit => if(u.value == 1) this else new TSUnitValuePair(value * u.value, new ScalarTSUnit())
		case u: TSUnit => this
	}
}