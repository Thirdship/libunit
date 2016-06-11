package com.thirdship.libunit.units

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.{UnitParser, TSUnitValuePair, AStarConvertibleTSUnitData, ScalarConversionEdge}
import org.scalatest.{FlatSpec, Matchers}

class MetricPrefixesTest extends FlatSpec with Matchers {

  private val baseUnit = "s".i

  var compressedParseMap = Map(
    baseUnit 	-> List("sec".i, "second".w),
    "min".i   	-> List("minute".w),
    "h".i 		  -> List("hr".w, "hour".w),
    "d".i 		  -> List("day".w),
    "y".i 		  -> List("yr".w, "year".w),
    "jiffy".i 	-> List()
  )

  var edges = List(
    new ScalarConversionEdge("min", 	  baseUnit.baseString,	60,       0.1),
    new ScalarConversionEdge("h",	 	    "min",		60,       0.1),
    new ScalarConversionEdge("d", 		  "h",		  24,       0.1),
    new ScalarConversionEdge("y", 		  "d",		  365.2422,	1),
    new ScalarConversionEdge(baseUnit.baseString, 	"jiffy",	3e-24,  	1)
  )

  "A MetricPrefixes" should "construct properly" in {
    """new MetricPrefixes(baseUnit,compressedParseMap(baseUnit))""" should compile
  }

  it should "augment compressedParseMap" in {
    val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Time", compressedParseMap, edges).createMetricUnits(List(baseUnit))

    println(data.compressedParseMap.keys.map(_.baseString))

    data.compressedParseMap.apply("Es".i).map(_.baseString).toSet  should be (Set("Esec","Esecond","exas","exasec","exasecond"))
    data.compressedParseMap.apply("Ps".e).map(_.baseString).toSet  should be (Set("Psec","Psecond","petas","petasec","petasecond"))
    data.compressedParseMap.apply("Ts".i).map(_.baseString).toSet  should be (Set("Tsec","Tsecond","teras","terasec","terasecond"))
    data.compressedParseMap.apply("Gs".i).map(_.baseString).toSet  should be (Set("Gsec","Gsecond","gigas","gigasec","gigasecond"))
    data.compressedParseMap.apply("Ms".e).map(_.baseString).toSet  should be (Set("Msec","Msecond","megas","megasec","megasecond"))
    data.compressedParseMap.apply("ks".i).map(_.baseString).toSet  should be (Set("ksec","ksecond","kilos","kilosec","kilosecond"))
    data.compressedParseMap.apply("hs".i).map(_.baseString).toSet  should be (Set("hsec","hsecond","hectos","hectosec","hectosecond"))
    data.compressedParseMap.apply("das".i).map(_.baseString).toSet should be (Set("dasec","dasecond","decas","decasec","decasecond"))
    data.compressedParseMap.apply("ds".i).map(_.baseString).toSet  should be (Set("dsec","dsecond","decis","decisec","decisecond"))
    data.compressedParseMap.apply("cs".i).map(_.baseString).toSet  should be (Set("csec","csecond","centis","centisec","centisecond"))
    data.compressedParseMap.apply("ms".e).map(_.baseString).toSet  should be (Set("msec","msecond","millis","millisec","millisecond"))
    data.compressedParseMap.apply("us".i).map(_.baseString).toSet  should be (Set("usec","usecond","μs","μsec","μsecond","micros","microsec","microsecond"))
    data.compressedParseMap.apply("ns".i).map(_.baseString).toSet  should be (Set("nsec","nsecond","nanos","nanosec","nanosecond"))
    data.compressedParseMap.apply("ps".e).map(_.baseString).toSet  should be (Set("psec","psecond","picos","picosec","picosecond"))
    data.compressedParseMap.apply("fs".i).map(_.baseString).toSet  should be (Set("fsec","fsecond","femtos","femtosec","femtosecond"))
    data.compressedParseMap.apply("as".i).map(_.baseString).toSet  should be (Set("asec","asecond","attos","attosec","attosecond"))
  }

  it should "augment edges" in {
    val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Time", compressedParseMap, edges).createMetricUnits(List(baseUnit))

    println(data.conversionEdges)

    data.aStar.getConversions(baseUnit.baseString, "Es")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "Es",  1e-18, 0.1)))
    data.aStar.getConversions(baseUnit.baseString, "Ps")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "Ps",  1e-15, 0.1)))
    data.aStar.getConversions(baseUnit.baseString, "Ts")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "Ts",  1e-12, 0.1)))
    data.aStar.getConversions(baseUnit.baseString, "Gs")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "Gs",  1e-9,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "Ms")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "Ms",  1e-6,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "ks")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "ks",  1e-3,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "hs")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "hs",  1e-2,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "das") should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "das", 1e-1,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "ds")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "ds",  1e1,   0.1)))
    data.aStar.getConversions(baseUnit.baseString, "cs")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "cs",  1e2,   0.1)))
    data.aStar.getConversions(baseUnit.baseString, "ms")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "ms",  1e3,   0.1)))
    data.aStar.getConversions(baseUnit.baseString, "us")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "us",  1e6,   0.1)))
    data.aStar.getConversions(baseUnit.baseString, "ns")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "ns",  1e9,   0.1)))
    data.aStar.getConversions(baseUnit.baseString, "ps")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "ps",  1e12,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "fs")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "fs",  1e15,  0.1)))
    data.aStar.getConversions(baseUnit.baseString, "as")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "as",  1e18,  0.1)))
  }

  it should "convert between metric and non-metric units" in {
    val ks = new TSUnitValuePair(1, new TimeTSUnit("ks"))
    val s  = new TSUnitValuePair(1, new TimeTSUnit("s" ))
    val ms = new TSUnitValuePair(1, new TimeTSUnit("ms"))
    val us = new TSUnitValuePair(1, new TimeTSUnit("us"))

    println(ks + s)
    println(s - us)
    println(s - ms)
  }

  it should "compare metric units correctly" in {
    List(
      ("10 ms","10000 us"),
      ("1000 seconds","1 kilosec"),
      ("1 sec","1000000 usecs"),
      ("1 Es","1 exaseconds"),
      ("1 hectosecs","10000 centis")
    ).foreach(s => UnitParser(s._1) == UnitParser(s._2) shouldBe true)
  }
}
