package com.thirdship.libunit

import scala.util.Random

import org.scalatest.{FlatSpec, Matchers}

class UnitConversionTest extends FlatSpec with Matchers {
  val alpha: String = "alpha"
  val beta: String = "beta"
  val gamma: String = "gamma"
  val delta: String = "delta"
  val epsilon: String = "epsilon"

  val factor1: Double = Random.nextDouble()
  val cost1: Double = Random.nextDouble()

  val factor2: Double = Random.nextDouble()
  val cost2: Double = Random.nextDouble()

  val factor3: Double = Random.nextDouble()
  val cost3: Double = Random.nextDouble()

  val allUnits = List(alpha, beta, gamma, delta, epsilon)

  "UnitConversion" should "play well with maps" in {
    Map(alpha -> factor1)(alpha) should equal(factor1)
  }

  it should "make new edges" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)

    // start units
    edgeA.start shouldBe beta
    edgeB.start shouldBe gamma
    edgeC.start shouldBe gamma
    // end units
    edgeA.end shouldBe alpha
    edgeB.end shouldBe alpha
    edgeC.end shouldBe delta
    // factors
    edgeA.conversion.to(1) shouldBe factor1
    edgeB.conversion.to(1) shouldBe factor2
    edgeC.conversion.to(1) shouldBe factor3
    // costs
    edgeA.cost shouldBe cost1
    edgeB.cost shouldBe cost2
    edgeC.cost shouldBe cost3
  }

  it should "find a cached edge" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    solver.getConversions(beta, alpha) shouldBe Some(edgeA)
    solver.getConversions(gamma, alpha) shouldBe Some(edgeB)
    solver.getConversions(gamma, delta) shouldBe Some(edgeC)
  }

  it should "find a reflexive edge" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    List(alpha, beta, gamma, delta, epsilon).foreach(u => {
      solver.getConversions(u, u).get.conversion.to(1) shouldBe 1
    })
  }

  it should "find a commutative edge" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    // alpha -> beta
    val ab = solver.getConversions(alpha, beta).get
    ab.conversion.to(1) shouldBe 1/ factor1
    ab.cost shouldBe cost1 + 1

    // alpha -> gamma
    val ag = solver.getConversions(alpha, gamma).get
    ag.conversion.to(1) shouldBe 1/ factor2
    ag.cost shouldBe cost2 + 1

    // delta -> gamma
    val dg = solver.getConversions(delta, gamma).get
    dg.conversion.to(1) shouldBe 1/ factor3
    dg.cost shouldBe cost3 + 1
  }

  it should "find neighbors" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    solver.getNeighbors(alpha)   shouldBe List(beta, gamma)
    solver.getNeighbors(beta)    shouldBe List(alpha)
    solver.getNeighbors(gamma)   shouldBe List(alpha, delta)
    solver.getNeighbors(delta)   shouldBe List(gamma)
    solver.getNeighbors(epsilon) shouldBe List.empty[BaseUnit]
  }

  it should "return  a real cost" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    // cached costs
    solver.heuristic(beta, alpha) should  be(cost1)
    solver.heuristic(gamma, alpha) shouldBe cost2
    solver.heuristic(gamma, delta) shouldBe cost3
    // commutative costs
    solver.heuristic(alpha, beta) should  be(cost1 + 1)
    solver.heuristic(alpha, gamma) shouldBe cost2 + 1
    solver.heuristic(delta, gamma) shouldBe cost3 + 1
    // reflexive costs
    solver.heuristic(alpha, alpha) should     be(0)
    solver.heuristic(beta, beta) should       be(0)
    solver.heuristic(gamma, gamma) should     be(0)
    solver.heuristic(delta, delta) should     be(0)
    solver.heuristic(epsilon, epsilon) shouldBe 0
  }

  it should "reconstruct a path" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))
    val cameFrom = Map[String, String](beta -> alpha, gamma -> alpha, delta -> gamma)

    val ad = solver.reconstructPath(cameFrom, alpha, delta)
    ad.cost shouldBe cost2 + 1 + cost3
    ad.conversion.to(1) shouldBe (1 / factor2) * factor3

    // TODO ensure this test makes sense (was not previously asserting anything)
    solver.allConversions shouldEqual solver.allConversions.filterNot(a => (a.start == alpha) && (a.end == delta))
  }

  it should "search unit map and find conversions" in {
    val edgeA = new ScalarConversionEdge(beta, alpha, factor1, cost1)
    val edgeB = new ScalarConversionEdge(gamma, alpha, factor2, cost2)
    val edgeC = new ScalarConversionEdge(gamma, delta, factor3, cost3)
    val solver = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    val ad = solver.solve(alpha, delta)
    ad.cost shouldBe cost2 + 1 + cost3
    ad.conversion.to(1) shouldBe (1/ factor2) * factor3
    // TODO ensure this test makes sense (was not previously asserting anything)
    solver.allConversions shouldEqual solver.allConversions.filterNot(a => (a.start == alpha) && (a.end == delta))

    val ab = solver.solve(alpha, beta)
    ab.cost shouldBe cost1 + 1
    ab.conversion.to(1) shouldBe 1/ factor1
    solver.allConversions shouldEqual solver.allConversions.filterNot(a => (a.start == alpha) && (a.end == beta))

    // TODO test for None or through exceptions
    val ea = solver.solve(epsilon, alpha)
    ea.start shouldBe "Failed A*"
    ea.end shouldBe "Failed A*"
    ea.cost shouldBe 0
    ea.conversion.to(1) shouldBe 1

    val gg = solver.solve(gamma, gamma)
    gg.cost shouldBe 0
    gg.conversion.to(1) shouldBe 1
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

    val solver = AStarSolver(units, edges)

    // n -> p
    // scalastyle:off magic.number
    val np = solver.solve("n", "p").conversion
    np.to(10) shouldBe wonkyConversion.to(10)
    np.to(0) shouldBe wonkyConversion.to(0)
    np.to(-10) shouldBe wonkyConversion.to(-10)
    np.from(10) shouldBe wonkyConversion.from(10)
    np.from(0) shouldBe wonkyConversion.from(0)
    np.from(-10) shouldBe wonkyConversion.from(-10)

    // kn -> n
    val knn = solver.solve("kn", "n").conversion
    knn.to(1) shouldBe 1000
    knn.to(.001) shouldBe 1
    knn.to(0) shouldBe 0

    // kn -> p
    val knp = solver.solve("kn", "p").conversion
    knp.to(1) shouldBe 5001
    knp.from(5001) shouldBe 1

    // kn -> kp
    val knkp = solver.solve("kn", "kp")
    knkp.conversion.to(1) shouldBe 5.001
    knkp.conversion.from(5.001) shouldBe 1
    knkp.cost shouldEqual 2.0

    // kp -> kn
    val kpkn = solver.solve("kp", "kn")
    kpkn.conversion.to(5.001) shouldBe 1
    kpkn.conversion.from(1) shouldBe 5.001
    kpkn.cost shouldEqual 3.0
    // scalastyle:on magic.number
  }
}
