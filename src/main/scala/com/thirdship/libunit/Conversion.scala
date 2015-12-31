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