package com.thirdship.libunit


/**
 * Holds two functions that operate on a given value of type M and transform it into another type N
 * @param toBaseUnit converts from M -> N
 * @param fromBaseUnit converts from N -> M
 * @tparam M the source type
 * @tparam N the destination/base storage type.
 *
 * @note the default values are to simply cast the input to the output type. This will surly break if M != N.
 */
case class Conversion[M,N](		toBaseUnit:   (M) => N = (a: M) => a.asInstanceOf[N], fromBaseUnit: (N) => M = (b: N) => b.asInstanceOf[M])

/**
 * Holds a conversion that can scale a Double to or from a certain scalar.
 *
 * @example
 * {{{
 *     val s = new ScalarConversion(10)
 *     s.toBaseUnit(10) 	//100
 *     s.fromBaseUnit(100) 	//10
 * }}}
 *
 * @param scalar the Double to scale by
 */
class ScalarConversion(scalar: Double = 1) extends Conversion[Double,Double]( (a:Double) => a * scalar, (a:Double) => a / scalar)


/**
  * A class of conversions, implemented as edges in the unit-conversion graph.
  * These edges are treated as being directed, carrying both the conversion factor and the cost of converting.
  *
  * @param start  The unit being converted from.
  * @param end    The unit being converted to.
  * @param factor The conversion factor from start to end.
  * @param cost   A measure of the precision cost form using this conversion from start to end.
  */
case class ConversionEdge(start: String, end: String, factor: Double, cost: Double){

	/**
	  * A commuting method for ConversionEdge. This is used to get the inverse conversion between units without needing to store redundant ConversionEdges.
	  *
	  * @return A ConversionEdge with reversed endpoints, the inverse conversion factor, and cost increased by one.
	  */
	def inverted: ConversionEdge = new ConversionEdge(end,start,1/factor,cost+1)
}
