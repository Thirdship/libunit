package com.thirdship.libunit

import com.thirdship.libunit.utils.ExactString
import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random

/**
  * Created by blazeutz on 2/12/16.
  */
class TSUnitConversionTest extends FlatSpec with Matchers {

  val alpha = "alpha"
  val beta = "beta"
  val gamma = "gamma"
  val delta = "delta"
  val epsilon = "epsilon"
  val fctr1 =  Random.nextDouble()
  val cst1 =   Random.nextDouble()
  val fctr2 =  Random.nextDouble()
  val cst2 =   Random.nextDouble()
  val fctr3 =  Random.nextDouble()
  val cst3 =   Random.nextDouble()

  val allUnits = List(alpha,beta,gamma,delta,epsilon)

  "TSUnitConversion" should "play well with maps" in {
    Map(alpha -> fctr1)(alpha) should equal(fctr1)
  }

  it should "make new edges" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)

    //start units
    edgeA.start should be(beta)
    edgeB.start should be(gamma)
    edgeC.start should be(gamma)
    //end units
    edgeA.end should be(alpha)
    edgeB.end should be(alpha)
    edgeC.end should be(delta)
    //factors
    edgeA.factor should be(fctr1)
    edgeB.factor should be(fctr2)
    edgeC.factor should be(fctr3)
    //costs
    edgeA.cost should be(cst1)
    edgeB.cost should be(cst2)
    edgeC.cost should be(cst3)
  }

  it should "find a cached edge" in {

    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))


    astar.getConversions(beta,alpha) should  be(Some(edgeA))
    astar.getConversions(gamma,alpha) should be(Some(edgeB))
    astar.getConversions(gamma,delta) should be(Some(edgeC))
  }

  it should "find a reflexive edge" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    astar.getConversions(alpha,alpha) should     be(Some(new ConversionEdge(alpha,alpha,1,0)))
    astar.getConversions(beta,beta) should       be(Some(new ConversionEdge(beta,beta,1,0)))
    astar.getConversions(gamma,gamma) should     be(Some(new ConversionEdge(gamma,gamma,1,0)))
    astar.getConversions(delta,delta) should     be(Some(new ConversionEdge(delta,delta,1,0)))
    astar.getConversions(epsilon,epsilon) should be(Some(new ConversionEdge(epsilon,epsilon,1,0)))
  }

  it should "find a commutative edge" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    astar.getConversions(alpha,beta) should  be(Some(new ConversionEdge(alpha,beta,1/fctr1,cst1+1)))
    astar.getConversions(alpha,gamma) should be(Some(new ConversionEdge(alpha,gamma,1/fctr2,cst2+1)))
    astar.getConversions(delta,gamma) should be(Some(new ConversionEdge(delta,gamma,1/fctr3,cst3+1)))
  }

  it should "find neighbors" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    astar.getNeighbors(alpha)   should be(List(beta,gamma))
    astar.getNeighbors(beta)    should be(List(alpha))
    astar.getNeighbors(gamma)   should be(List(alpha,delta))
    astar.getNeighbors(delta)   should be(List(gamma))
    astar.getNeighbors(epsilon) should be(List.empty[TSUnit])
  }

  it should "return  a real cost" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    //cached costs
    astar.heuristic(beta,alpha) should  be(cst1)
    astar.heuristic(gamma,alpha) should be(cst2)
    astar.heuristic(gamma,delta) should be(cst3)
    //commutative costs
    astar.heuristic(alpha,beta) should  be(cst1+1)
    astar.heuristic(alpha,gamma) should be(cst2+1)
    astar.heuristic(delta,gamma) should be(cst3+1)
    //reflexive costs
    astar.heuristic(alpha,alpha) should     be(0)
    astar.heuristic(beta,beta) should       be(0)
    astar.heuristic(gamma,gamma) should     be(0)
    astar.heuristic(delta,delta) should     be(0)
    astar.heuristic(epsilon,epsilon) should be(0)
  }

  it should "reconstruct a path" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))
    val cameFrom: Map[String, String] = Map(beta -> alpha,gamma -> alpha,delta -> gamma)

    astar.reconstructPath(cameFrom,alpha,delta) should be(new ConversionEdge(alpha,delta,(1/fctr2)*fctr3,cst2+1+cst3))
    astar.allConversions = astar.allConversions.filterNot(a => (a.start == alpha) && (a.end == delta))
  }

  it should "search unit map and find conversions" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    val astar = AStarSolver(allUnits, List(edgeA, edgeB, edgeC))

    //println("Instantiation passed!")
    astar.solve(alpha,delta) should be(new ConversionEdge(alpha,delta,(1/fctr2)*fctr3,cst2+1+cst3))
    astar.allConversions = astar.allConversions.filterNot(a => (a.start == alpha) && (a.end == delta))
    //println("Searching path passed!")
    astar.solve(alpha,beta) should be(new ConversionEdge(alpha,beta,1/fctr1,cst1+1))
    astar.allConversions = astar.allConversions.filterNot(a => (a.start == alpha) && (a.end == beta))
    //println("Commutative path passed!")
    // TODO test for None or through exceptions
    astar.solve(epsilon,alpha) should be(new ConversionEdge("Failed A*","Failed A*",1,0))
    //println("Null path passed!")
    astar.solve(gamma,gamma) should be(new ConversionEdge(gamma,gamma,1,0))
    //println("Reflexive path passed!")
  }

}
