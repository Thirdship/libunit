package com.thirdship.libunit.units

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.{AStarConvertibleTSUnitData, ScalarConversionEdge}
import org.scalatest.{FlatSpec, Matchers}

class MetricTSUnitTest extends FlatSpec with Matchers {

  private val baseUnit = "s"

  private val baseParseList = List("sec".i, "second".w)

  var compressedParseMap = Map(
    baseUnit.i 	-> baseParseList,
    "min".i 	-> List("minute".w),
    "h".i 		-> List("hr".w, "hour".w),
    "d".i 		-> List("day".w),
    "y".i 		-> List("yr".w, "year".w),
    "jiffy".i 	-> List()
  )

  var edges = List(
    new ScalarConversionEdge(baseUnit,	baseUnit,	1),
    new ScalarConversionEdge("min", 	baseUnit,	60),
    new ScalarConversionEdge("h",	 	"min",		60),
    new ScalarConversionEdge("d", 		"h",		24),
    new ScalarConversionEdge("y", 		"d",		365.2422,	1),
    new ScalarConversionEdge(baseUnit, 	"jiffy",	3e-24,		1)
  )

  "A MetricTSUnit" should "construct properly" in {
    """new MetricTSUnit(baseUnit,baseParseList)""" should compile
  }

  it should "augment compressedParseMap" in {
    val lore: MetricTSUnit = new MetricTSUnit(baseUnit,baseParseList)
    compressedParseMap ++= lore.compressedParseMap

    println(compressedParseMap.keys.map(_.baseString))

    println("Test Es")
    compressedParseMap.apply("Es".i).map(_.baseString).toSet  should be (Set("Esec","Esecond","exas","exasec","exasecond"))
    println("Test Ps")
    compressedParseMap.apply("Ps".e).map(_.baseString).toSet  should be (Set("Psec","Psecond","petas","petasec","petasecond"))
    println("Test Ts")
    compressedParseMap.apply("Ts".i).map(_.baseString).toSet  should be (Set("Tsec","Tsecond","teras","terasec","terasecond"))
    println("Test Gs")
    compressedParseMap.apply("Gs".i).map(_.baseString).toSet  should be (Set("Gsec","Gsecond","gigas","gigasec","gigasecond"))
    println("Test Ms")
    compressedParseMap.apply("Ms".e).map(_.baseString).toSet  should be (Set("Msec","Msecond","megas","megasec","megasecond"))
    println("Test ks")
    compressedParseMap.apply("ks".i).map(_.baseString).toSet  should be (Set("ksec","ksecond","kilos","kilosec","kilosecond"))
    println("Test hs")
    compressedParseMap.apply("hs".i).map(_.baseString).toSet  should be (Set("hsec","hsecond","hectos","hectosec","hectosecond"))
    println("Test das")
    compressedParseMap.apply("das".i).map(_.baseString).toSet should be (Set("dasec","dasecond","decas","decasec","decasecond"))
    println("Test ds")
    compressedParseMap.apply("ds".i).map(_.baseString).toSet  should be (Set("dsec","dsecond","decis","decisec","decisecond"))
    println("Test cs")
    compressedParseMap.apply("cs".i).map(_.baseString).toSet  should be (Set("csec","csecond","centis","centisec","centisecond"))
    println("Test ms")
    compressedParseMap.apply("ms".e).map(_.baseString).toSet  should be (Set("msec","msecond","millis","millisec","millisecond"))
    println("Test us")
    compressedParseMap.apply("us".i).map(_.baseString).toSet  should be (Set("usec","usecond","micros","microsec","microsecond"))
    println("Test ns")
    compressedParseMap.apply("ns".i).map(_.baseString).toSet  should be (Set("nsec","nsecond","nanos","nanosec","nanosecond"))
    println("Test ps")
    compressedParseMap.apply("ps".e).map(_.baseString).toSet  should be (Set("psec","psecond","picos","picosec","picosecond"))
    println("Test fs")
    compressedParseMap.apply("fs".i).map(_.baseString).toSet  should be (Set("fsec","fsecond","femtos","femtosec","femtosecond"))
    println("Test as")
    compressedParseMap.apply("as".i).map(_.baseString).toSet  should be (Set("asec","asecond","attos","attosec","attosecond"))
  }

  it should "augment edges" in {
    val lore: MetricTSUnit = new MetricTSUnit(baseUnit,baseParseList)
    compressedParseMap ++= lore.compressedParseMap
    edges ++= lore.edges
    val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Time", compressedParseMap, edges)

    println(edges)

    println("Test Es")
    data.aStar.getConversions(baseUnit, "Es") should be (Option(new ScalarConversionEdge[String](baseUnit, "Es", 1e-18, 0.1)))
    println("Test Ps")
    data.aStar.getConversions(baseUnit, "Ps") should be (Option(new ScalarConversionEdge[String](baseUnit, "Ps", 1e-15, 0.1)))
    println("Test Ts")
    data.aStar.getConversions(baseUnit, "Ts") should be (Option(new ScalarConversionEdge[String](baseUnit, "Ts", 1e-12, 0.1)))
    println("Test Gs")
    data.aStar.getConversions(baseUnit, "Gs") should be (Option(new ScalarConversionEdge[String](baseUnit, "Gs", 1e-9, 0.1)))
    println("Test Ms")
    data.aStar.getConversions(baseUnit, "Ms") should be (Option(new ScalarConversionEdge[String](baseUnit, "Ms", 1e-6, 0.1)))
    println("Test ks")
    data.aStar.getConversions(baseUnit, "ks") should be (Option(new ScalarConversionEdge[String](baseUnit, "ks", 1e-3, 0.1)))
    println("Test hs")
    data.aStar.getConversions(baseUnit, "hs") should be (Option(new ScalarConversionEdge[String](baseUnit, "hs", 1e-2, 0.1)))
    println("Test das")
    data.aStar.getConversions(baseUnit, "das") should be (Option(new ScalarConversionEdge[String](baseUnit, "das", 1e-1, 0.1)))
    println("Test ds")
    data.aStar.getConversions(baseUnit, "ds") should be (Option(new ScalarConversionEdge[String](baseUnit, "ds", 1e1, 0.1)))
    println("Test cs")
    data.aStar.getConversions(baseUnit, "cs") should be (Option(new ScalarConversionEdge[String](baseUnit, "cs", 1e2, 0.1)))
    println("Test ms")
    data.aStar.getConversions(baseUnit, "ms") should be (Option(new ScalarConversionEdge[String](baseUnit, "ms", 1e3, 0.1)))
    println("Test us")
    data.aStar.getConversions(baseUnit, "us") should be (Option(new ScalarConversionEdge[String](baseUnit, "us", 1e6, 0.1)))
    println("Test ns")
    data.aStar.getConversions(baseUnit, "ns") should be (Option(new ScalarConversionEdge[String](baseUnit, "ns", 1e9, 0.1)))
    println("Test ps")
    data.aStar.getConversions(baseUnit, "ps") should be (Option(new ScalarConversionEdge[String](baseUnit, "ps", 1e12, 0.1)))
    println("Test fs")
    data.aStar.getConversions(baseUnit, "fs") should be (Option(new ScalarConversionEdge[String](baseUnit, "fs", 1e15, 0.1)))
    println("Test as")
    data.aStar.getConversions(baseUnit, "as") should be (Option(new ScalarConversionEdge[String](baseUnit, "as", 1e18, 0.1)))
  }
}
