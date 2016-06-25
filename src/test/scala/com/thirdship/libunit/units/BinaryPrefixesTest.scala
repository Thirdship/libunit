package com.thirdship.libunit.units

import com.thirdship.libunit.utils.Helpers._
import com.thirdship.libunit.{UnitParser, TSUnitValuePair, AStarConvertibleTSUnitData, ScalarConversionEdge}
import org.scalatest.{FlatSpec, Matchers}

class BinaryPrefixesTest extends FlatSpec with Matchers {

	private val baseUnit = "B".e

	var compressedParseMap = Map(
		baseUnit 	-> List("byte".i),
		"b".e       -> List("bit".i),
		"nibble".i  -> List("nybble".i, "nyble".i, "half-byte".i)
	)

	var edges = List(
		new ScalarConversionEdge(baseUnit.baseString, "b",	8,       0.1),
		new ScalarConversionEdge(baseUnit.baseString, "nibble", 2, 0.1)
	)

	"A BinaryPrefixes" should "construct properly" in {
		"""new MetricPrefixes(baseUnit,compressedParseMap(baseUnit))""" should compile
	}

	it should "augment compressedParseMap" in {
		val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Storage", compressedParseMap, edges).createMetricUnits(List(baseUnit, "b".e)).createBinaryUnits(List(baseUnit, "b".e))

		println(data.compressedParseMap.keys.map(_.baseString))

		data.compressedParseMap.apply("YiB".e).map(_.baseString).toSet  should be (Set("Yibyte","yobiB","yobibyte"))
		data.compressedParseMap.apply("ZiB".e).map(_.baseString).toSet  should be (Set("Zibyte","zebiB","zebibyte"))
		data.compressedParseMap.apply("EiB".e).map(_.baseString).toSet  should be (Set("Eibyte","exbiB","exbibyte"))
		data.compressedParseMap.apply("PiB".e).map(_.baseString).toSet  should be (Set("Pibyte","pebiB","pebibyte"))
		data.compressedParseMap.apply("TiB".e).map(_.baseString).toSet  should be (Set("Tibyte","tebiB","tebibyte"))
		data.compressedParseMap.apply("GiB".e).map(_.baseString).toSet  should be (Set("Gibyte","gibiB","gibibyte"))
		data.compressedParseMap.apply("MiB".e).map(_.baseString).toSet  should be (Set("Mibyte","mebiB","mebibyte"))
		data.compressedParseMap.apply("KiB".e).map(_.baseString).toSet  should be (Set("Kibyte","kibiB","kibibyte"))

		data.compressedParseMap.apply("Yib".e).map(_.baseString).toSet  should be (Set("Yibit","yobib","yobibit"))
		data.compressedParseMap.apply("Zib".e).map(_.baseString).toSet  should be (Set("Zibit","zebib","zebibit"))
		data.compressedParseMap.apply("Eib".e).map(_.baseString).toSet  should be (Set("Eibit","exbib","exbibit"))
		data.compressedParseMap.apply("Pib".e).map(_.baseString).toSet  should be (Set("Pibit","pebib","pebibit"))
		data.compressedParseMap.apply("Tib".e).map(_.baseString).toSet  should be (Set("Tibit","tebib","tebibit"))
		data.compressedParseMap.apply("Gib".e).map(_.baseString).toSet  should be (Set("Gibit","gibib","gibibit"))
		data.compressedParseMap.apply("Mib".e).map(_.baseString).toSet  should be (Set("Mibit","mebib","mebibit"))
		data.compressedParseMap.apply("Kib".e).map(_.baseString).toSet  should be (Set("Kibit","kibib","kibibit"))
	}

	it should "augment edges" in {
		val data: AStarConvertibleTSUnitData = new AStarConvertibleTSUnitData(baseUnit, "Storage", compressedParseMap, edges).createMetricUnits(List(baseUnit, "b".e)).createBinaryUnits(List(baseUnit, "b".e))

		println(data.conversionEdges)

		data.aStar.getConversions(baseUnit.baseString, "YiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "YiB", Math.pow(2, 80), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "ZiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "ZiB", Math.pow(2, 70), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "EiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "EiB", Math.pow(2, 60), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "PiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "PiB", Math.pow(2, 50), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "TiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "TiB", Math.pow(2, 40), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "GiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "GiB", Math.pow(2, 30), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "MiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "MiB", Math.pow(2, 20), 0.1)))
		data.aStar.getConversions(baseUnit.baseString, "KiB")  should be (Option(new ScalarConversionEdge[String](baseUnit.baseString, "KiB", Math.pow(2, 10), 0.1)))

		data.aStar.getConversions("b", "Yib")  should be (Option(new ScalarConversionEdge[String]("b", "Yib", Math.pow(2, 80), 0.1)))
		data.aStar.getConversions("b", "Zib")  should be (Option(new ScalarConversionEdge[String]("b", "Zib", Math.pow(2, 70), 0.1)))
		data.aStar.getConversions("b", "Eib")  should be (Option(new ScalarConversionEdge[String]("b", "Eib", Math.pow(2, 60), 0.1)))
		data.aStar.getConversions("b", "Pib")  should be (Option(new ScalarConversionEdge[String]("b", "Pib", Math.pow(2, 50), 0.1)))
		data.aStar.getConversions("b", "Tib")  should be (Option(new ScalarConversionEdge[String]("b", "Tib", Math.pow(2, 40), 0.1)))
		data.aStar.getConversions("b", "Gib")  should be (Option(new ScalarConversionEdge[String]("b", "Gib", Math.pow(2, 30), 0.1)))
		data.aStar.getConversions("b", "Mib")  should be (Option(new ScalarConversionEdge[String]("b", "Mib", Math.pow(2, 20), 0.1)))
		data.aStar.getConversions("b", "Kib")  should be (Option(new ScalarConversionEdge[String]("b", "Kib", Math.pow(2, 10), 0.1)))
	}

	// TODO Add conversion tests between binary and non-binary units once StorageTSUnit is created

	it should "compare binary units correctly" in {
		List(
			("2048 Kib","2 Mib"),
			("3 TiB","3145728 MiB"),
			("8192 Zebibits","1 Yibyte")
		).foreach(s => UnitParser(s._1) == UnitParser(s._2) shouldBe true)
	}
}
