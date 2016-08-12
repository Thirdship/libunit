package com.thirdship.libunit

import scala.util.Random

import org.scalatest.{FlatSpec, Matchers}

class TSUnitConversionTest extends FlatSpec with Matchers {

  val alpha = "alpha"
  val beta = "beta"
  val gamma = "gamma"
  val delta = "delta"
  val epsilon = "epsilon"
  val fctr1 = Random.nextDouble()
  val cst1 = Random.nextDouble()
  val fctr2 = Random.nextDouble()
  val cst2 = Random.nextDouble()
  val fctr3 = Random.nextDouble()
  val cst3 = Random.nextDouble()

  val allUnits = List(alpha, beta, gamma, delta, epsilon)

  "TSUnitConversion" should "play well with maps" in {
    Map(alpha -> fctr1)(alpha) should equal(fctr1)
  }

  it should "make new edges" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)

    // start units
    edgeA.start should be(beta)
    edgeB.start should be(gamma)
    edgeC.start should be(gamma)
    // end units
    edgeA.end should be(alpha)
    edgeB.end should be(alpha)
    edgeC.end should be(delta)
    // factors
    edgeA.conversion.to(1) should be(fctr1)
    edgeB.conversion.to(1) should be(fctr2)
    edgeC.conversion.to(1) should be(fctr3)
    // costs
    edgeA.cost should be(cst1)
    edgeB.cost should be(cst2)
    edgeC.cost should be(cst3)
  }

  it should "find a cached edge" in {

    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))


    astar.getConversions(beta, alpha) should  be(Some(edgeA))
    astar.getConversions(gamma, alpha) should be(Some(edgeB))
    astar.getConversions(gamma, delta) should be(Some(edgeC))
  }

  it should "find a reflexive edge" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    List(alpha, beta, gamma, delta, epsilon).foreach(u => {
      astar.getConversions(u, u).get.conversion.to(1) should be(1)
    })
  }

  it should "find a commutative edge" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    // alpha -> beta
    val ab = astar.getConversions(alpha, beta).get
    ab.conversion.to(1) should be(1/fctr1)
    ab.cost should be(cst1 + 1)

    // alpha -> gamma
    val ag = astar.getConversions(alpha, gamma).get
    ag.conversion.to(1) should be(1/fctr2)
    ag.cost should be(cst2 + 1)

    // delta -> gamma
    val dg = astar.getConversions(delta, gamma).get
    dg.conversion.to(1) should be(1/fctr3)
    dg.cost should be(cst3 + 1)

  }

  it should "find neighbors" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    astar.getNeighbors(alpha)   should be(List(beta, gamma))
    astar.getNeighbors(beta)    should be(List(alpha))
    astar.getNeighbors(gamma)   should be(List(alpha, delta))
    astar.getNeighbors(delta)   should be(List(gamma))
    astar.getNeighbors(epsilon) should be(List.empty[TSUnit])
  }

  it should "return  a real cost" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    // cached costs
    astar.heuristic(beta, alpha) should  be(cst1)
    astar.heuristic(gamma, alpha) should be(cst2)
    astar.heuristic(gamma, delta) should be(cst3)
    // commutative costs
    astar.heuristic(alpha, beta) should  be(cst1 + 1)
    astar.heuristic(alpha, gamma) should be(cst2 + 1)
    astar.heuristic(delta, gamma) should be(cst3 + 1)
    // reflexive costs
    astar.heuristic(alpha, alpha) should     be(0)
    astar.heuristic(beta, beta) should       be(0)
    astar.heuristic(gamma, gamma) should     be(0)
    astar.heuristic(delta, delta) should     be(0)
    astar.heuristic(epsilon, epsilon) should be(0)
  }

  it should "reconstruct a path" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))
    val cameFrom: Map[String, String] = Map(beta -> alpha, gamma -> alpha, delta -> gamma)

    val ad = astar.reconstructPath(cameFrom, alpha, delta)
    ad.cost === cst2 + 1 + cst3
    ad.conversion.to(1) === (1/fctr2)*fctr3

    astar.allConversions = astar.allConversions.filterNot(a => (a.start == alpha) && (a.end == delta))
  }

  it should "search unit map and find conversions" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, fctr1, cst1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, fctr2, cst2)
    val edgeC = new ScalarConversionEdge(gamma, delta, fctr3, cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    val ad = astar.solve(alpha, delta)
    ad.cost === cst2 + 1 + cst3
    ad.conversion.to(1) === (1/fctr2)*fctr3
    astar.allConversions = astar.allConversions.filterNot(a => (a.start == alpha) && (a.end == delta))

    val ab = astar.solve(alpha, beta)
    ab.cost === cst1 + 1
    ab.conversion.to(1) === 1/fctr1
    astar.allConversions = astar.allConversions.filterNot(a => (a.start == alpha) && (a.end == beta))

    // TODO test for None or through exceptions
    val ea = astar.solve(epsilon, alpha)
    ea.start === "Failed A*"
    ea.end === "Failed A*"
    ea.cost === 0
    ea.conversion.to(1) === 1

    val gg = astar.solve(gamma, gamma)
    gg.cost === 0
    gg.conversion.to(1) === 1
  }

  it should "properly handle nonlinear conversions" in {
    val units = List(
      "n", "kn",
      "p", "kp"
    )

    val wonkyConversion = Conversion((a: Double) => (5 * a) + 1, (a: Double) => (a -1) / 5)
    val edges = List(
      new ConversionEdge("n", "p", wonkyConversion, 1),
      new ScalarConversionEdge("n", "kn", .001),
      new ScalarConversionEdge("p", "kp", .001)
    )

    val astar = AStarSolver(units, edges)

    // n -> p
    // scalastyle:off magic.number
    val np = astar.solve("n", "p").conversion
    np.to(10) should be(wonkyConversion.to(10))
    np.to(0) should be(wonkyConversion.to(0))
    np.to(-10) should be(wonkyConversion.to(-10))
    np.from(10) should be(wonkyConversion.from(10))
    np.from(0) should be(wonkyConversion.from(0))
    np.from(-10) should be(wonkyConversion.from(-10))

    // kn -> n
    val knn = astar.solve("kn", "n").conversion
    knn.to(1) should be(1000)
    knn.to(.001) should be(1)
    knn.to(0) should be(0)

    // kn -> p
    val knp = astar.solve("kn", "p").conversion
    knp.to(1) should be(5001)
    knp.from(5001) should be(1)

    // kn -> kp
    val knkp = astar.solve("kn", "kp")
    knkp.conversion.to(1) should be(5.001)
    knkp.conversion.from(5.001) should be(1)
    knkp.cost === 2.0

    // kp -> kn
    val kpkn = astar.solve("kp", "kn")
    kpkn.conversion.to(5.001) should be(1)
    kpkn.conversion.from(1) should be(5.001)
    kpkn.cost === 3.0
    // scalastyle:on magic.number
  }

}
