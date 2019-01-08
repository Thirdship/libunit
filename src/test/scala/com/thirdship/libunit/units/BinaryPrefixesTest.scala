package com.thirdship.libunit.units

import org.scalatest.{FlatSpec, Matchers}

import com.thirdship.libunit._
import com.thirdship.libunit.utils.Helpers._

class BinaryPrefixesTest extends FlatSpec with Matchers {

	private val baseUnit = "B".e

	val compressedParseMap = Map(
		baseUnit -> List("byte".w),
		"b".e -> List("bit".w),
		"nibble".i -> List("nybble".w, "nyble".w, "half-byte".w)
	)

	val edges = List( // scalastyle:off magic.number
		new ScalarConversionEdge(baseUnit.baseString, "b", 8, ConversionEdge.LOW_COST_EDGE),
		new ScalarConversionEdge(baseUnit.baseString, "nibble", 2, ConversionEdge.LOW_COST_EDGE)
	) // scalastyle:on magic.number

	"A BinaryPrefixes" should "construct properly" in {
		"""new MetricPrefixes(baseUnit,compressedParseMap(baseUnit))""" should compile
	}

	it should "augment compressedParseMap" in {
		val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit, "Storage", compressedParseMap, edges)
			.createBinaryUnits(List(baseUnit, "b".e))

		println(data.compressedParseMap.keys.map(_.baseString)) // scalastyle:ignore println

		data.compressedParseMap.apply("YiB".e).map(_.baseString).toSet  should be (Set("Yibyte", "yobiB", "yobibyte"))
		data.compressedParseMap.apply("ZiB".e).map(_.baseString).toSet  should be (Set("Zibyte", "zebiB", "zebibyte"))
		data.compressedParseMap.apply("EiB".e).map(_.baseString).toSet  should be (Set("Eibyte", "exbiB", "exbibyte"))
		data.compressedParseMap.apply("PiB".e).map(_.baseString).toSet  should be (Set("Pibyte", "pebiB", "pebibyte"))
		data.compressedParseMap.apply("TiB".e).map(_.baseString).toSet  should be (Set("Tibyte", "tebiB", "tebibyte"))
		data.compressedParseMap.apply("GiB".e).map(_.baseString).toSet  should be (Set("Gibyte", "gibiB", "gibibyte"))
		data.compressedParseMap.apply("MiB".e).map(_.baseString).toSet  should be (Set("Mibyte", "mebiB", "mebibyte"))
		data.compressedParseMap.apply("KiB".e).map(_.baseString).toSet  should be (Set("Kibyte", "kibiB", "kibibyte"))

		data.compressedParseMap.apply("Yib".e).map(_.baseString).toSet  should be (Set("Yibit", "yobib", "yobibit"))
		data.compressedParseMap.apply("Zib".e).map(_.baseString).toSet  should be (Set("Zibit", "zebib", "zebibit"))
		data.compressedParseMap.apply("Eib".e).map(_.baseString).toSet  should be (Set("Eibit", "exbib", "exbibit"))
		data.compressedParseMap.apply("Pib".e).map(_.baseString).toSet  should be (Set("Pibit", "pebib", "pebibit"))
		data.compressedParseMap.apply("Tib".e).map(_.baseString).toSet  should be (Set("Tibit", "tebib", "tebibit"))
		data.compressedParseMap.apply("Gib".e).map(_.baseString).toSet  should be (Set("Gibit", "gibib", "gibibit"))
		data.compressedParseMap.apply("Mib".e).map(_.baseString).toSet  should be (Set("Mibit", "mebib", "mebibit"))
		data.compressedParseMap.apply("Kib".e).map(_.baseString).toSet  should be (Set("Kibit", "kibib", "kibibit"))
	}

	it should "augment edges" in {
		val data: AStarConvertibleUnitData = new AStarConvertibleUnitData(baseUnit, "Storage", compressedParseMap, edges)
			.createBinaryUnits(List(baseUnit, "b".e))

		println(data.conversionEdges) // scalastyle:ignore println
		val baseString = baseUnit.baseString

		// scalastyle:off magic.number
		data.aStar.getConversion(baseString, "YiB") should be (Option(new ScalarConversionEdge[String](baseString, "YiB", Math.pow(2, -80), 1)))
		data.aStar.getConversion(baseString, "ZiB") should be (Option(new ScalarConversionEdge[String](baseString, "ZiB", Math.pow(2, -70), 1)))
		data.aStar.getConversion(baseString, "EiB") should be (Option(new ScalarConversionEdge[String](baseString, "EiB", Math.pow(2, -60), 1)))
		data.aStar.getConversion(baseString, "PiB") should be (Option(new ScalarConversionEdge[String](baseString, "PiB", Math.pow(2, -50), 1)))
		data.aStar.getConversion(baseString, "TiB") should be (Option(new ScalarConversionEdge[String](baseString, "TiB", Math.pow(2, -40), 1)))
		data.aStar.getConversion(baseString, "GiB") should be (Option(new ScalarConversionEdge[String](baseString, "GiB", Math.pow(2, -30), 1)))
		data.aStar.getConversion(baseString, "MiB") should be (Option(new ScalarConversionEdge[String](baseString, "MiB", Math.pow(2, -20), 1)))
		data.aStar.getConversion(baseString, "KiB") should be (Option(new ScalarConversionEdge[String](baseString, "KiB", Math.pow(2, -10), 1)))

		data.aStar.getConversion("b", "Yib") should be (Option(new ScalarConversionEdge[String]("b", "Yib", Math.pow(2, -80), 1)))
		data.aStar.getConversion("b", "Zib") should be (Option(new ScalarConversionEdge[String]("b", "Zib", Math.pow(2, -70), 1)))
		data.aStar.getConversion("b", "Eib") should be (Option(new ScalarConversionEdge[String]("b", "Eib", Math.pow(2, -60), 1)))
		data.aStar.getConversion("b", "Pib") should be (Option(new ScalarConversionEdge[String]("b", "Pib", Math.pow(2, -50), 1)))
		data.aStar.getConversion("b", "Tib") should be (Option(new ScalarConversionEdge[String]("b", "Tib", Math.pow(2, -40), 1)))
		data.aStar.getConversion("b", "Gib") should be (Option(new ScalarConversionEdge[String]("b", "Gib", Math.pow(2, -30), 1)))
		data.aStar.getConversion("b", "Mib") should be (Option(new ScalarConversionEdge[String]("b", "Mib", Math.pow(2, -20), 1)))
		data.aStar.getConversion("b", "Kib") should be (Option(new ScalarConversionEdge[String]("b", "Kib", Math.pow(2, -10), 1)))
	} // scalastyle:on magic.number

	it should "convert between binary and non-binary units" in {
		val b = UnitValuePair(1, new StorageUnit("b"))
		val byte = UnitValuePair(1, new StorageUnit("B"))
		val kbyte = UnitValuePair(1, new StorageUnit("kB"))
		val mbyte = UnitValuePair(1, new StorageUnit("MB"))
		val kibyte = UnitValuePair(1, new StorageUnit("KiB"))
		val mibyte = UnitValuePair(1, new StorageUnit("MiB"))

		// scalastyle:off println
		println(b + byte)
		println(mbyte + kbyte)
		println(kbyte - byte)
		println(kibyte - mibyte)
		println(byte + kibyte)
	} // scalastyle:on println

	it should "compare binary units correctly" in {
		List(
			("2048 Kib", "2 Mib"),
			("3 TiB", "3145728 MiB"),
			("8192 Zebibits", "1 Yibyte")
		).foreach(s => UnitParser(s._1) == UnitParser(s._2) shouldBe true)
	}
}
