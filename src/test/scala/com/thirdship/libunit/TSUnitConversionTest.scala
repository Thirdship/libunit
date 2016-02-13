package com.thirdship.libunit

import com.thirdship.libunit.utils.ExactString
import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random

/**
  * Created by blazeutz on 2/12/16.
  */
class TSUnitConversionTest extends FlatSpec with Matchers {

  val alpha = new BaseTSUnit("alpha")
  val beta = new BaseTSUnit("beta")
  val gamma = new BaseTSUnit("gamma")
  val delta = new BaseTSUnit("delta")
  val epsilon = new BaseTSUnit("epsilon")
  val fctr1 =  Random.nextDouble()
  val cst1 =   Random.nextDouble()
  val fctr2 =  Random.nextDouble()
  val cst2 =   Random.nextDouble()
  val fctr3 =  Random.nextDouble()
  val cst3 =   Random.nextDouble()
  TSUnitConversion.allTSUnits ++= List(alpha,beta,gamma,delta,epsilon)

  "TSUnit" should "play well with maps" in {
    Map(alpha -> fctr1)(alpha) should equal(fctr1)
  }

  "ConversionEdge" should "make new edges" in {
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

  "searchConversions" should "find a cached edge" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)

    TSUnitConversion.searchConversions(beta,alpha) should  be(edgeA)
    TSUnitConversion.searchConversions(gamma,alpha) should be(edgeB)
    TSUnitConversion.searchConversions(gamma,delta) should be(edgeC)
  }

  "searchConversions" should "find a reflexive edge" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)

    TSUnitConversion.searchConversions(alpha,alpha) should     be(new ConversionEdge(alpha,alpha,1,0))
    TSUnitConversion.searchConversions(beta,beta) should       be(new ConversionEdge(beta,beta,1,0))
    TSUnitConversion.searchConversions(gamma,gamma) should     be(new ConversionEdge(gamma,gamma,1,0))
    TSUnitConversion.searchConversions(delta,delta) should     be(new ConversionEdge(delta,delta,1,0))
    TSUnitConversion.searchConversions(epsilon,epsilon) should be(new ConversionEdge(epsilon,epsilon,1,0))
  }

  "searchConversions" should "find a commutative edge" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)

    TSUnitConversion.searchConversions(alpha,beta) should  be(new ConversionEdge(alpha,beta,1/fctr1,cst1+1))
    TSUnitConversion.searchConversions(alpha,gamma) should be(new ConversionEdge(alpha,gamma,1/fctr2,cst2+1))
    TSUnitConversion.searchConversions(delta,gamma) should be(new ConversionEdge(delta,gamma,1/fctr3,cst3+1))
  }

  "searchNeighbors" should "find neighbors" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)

    TSUnitConversion.searchNeighbors(alpha)   should be(List(beta,gamma))
    TSUnitConversion.searchNeighbors(beta)    should be(List(alpha))
    TSUnitConversion.searchNeighbors(gamma)   should be(List(alpha,delta))
    TSUnitConversion.searchNeighbors(delta)   should be(List(gamma))
    TSUnitConversion.searchNeighbors(epsilon) should be(List.empty[TSUnit])
  }

  "heuristic" should "return  a real cost" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)

    //cached costs
    TSUnitConversion.heuristic(beta,alpha) should  be(cst1)
    TSUnitConversion.heuristic(gamma,alpha) should be(cst2)
    TSUnitConversion.heuristic(gamma,delta) should be(cst3)
    //commutative costs
    TSUnitConversion.heuristic(alpha,beta) should  be(cst1+1)
    TSUnitConversion.heuristic(alpha,gamma) should be(cst2+1)
    TSUnitConversion.heuristic(delta,gamma) should be(cst3+1)
    //reflexive costs
    TSUnitConversion.heuristic(alpha,alpha) should     be(0)
    TSUnitConversion.heuristic(beta,beta) should       be(0)
    TSUnitConversion.heuristic(gamma,gamma) should     be(0)
    TSUnitConversion.heuristic(delta,delta) should     be(0)
    TSUnitConversion.heuristic(epsilon,epsilon) should be(0)
  }

  "reconstructPath" should "do its job" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)
    val cameFrom: Map[TSUnit, TSUnit] = Map(alpha -> null,beta -> alpha,gamma -> alpha,delta -> gamma)

    TSUnitConversion.reconstructPath(cameFrom,delta,alpha,delta) should be(new ConversionEdge(alpha,delta,(1/fctr2)*fctr3,cst2+1+cst3))
  }

  "aStar" should "search unit map and find conversions" in {
    val edgeA = new ConversionEdge(beta,alpha,fctr1,cst1)
    val edgeB = new ConversionEdge(gamma,alpha,fctr2,cst2)
    val edgeC = new ConversionEdge(gamma,delta,fctr3,cst3)
    TSUnitConversion.allConversions ++= List(edgeA,edgeB,edgeC)

    //println("Instantiation passed!")
    TSUnitConversion.aStar(alpha,delta) should be(new ConversionEdge(alpha,delta,(1/fctr2)*fctr3,cst2+1+cst3))
    //println("Searching path passed!")
    TSUnitConversion.aStar(alpha,beta) should be(new ConversionEdge(alpha,beta,1/fctr1,cst1+1))
    //println("Commutative path passed!")
    TSUnitConversion.aStar(epsilon,alpha) should be(new ConversionEdge(new BaseTSUnit("Failed A*"),new BaseTSUnit("Failed A*"),1,0))
    //println("Null path passed!")
    TSUnitConversion.aStar(gamma,gamma) should be(new ConversionEdge(gamma,gamma,1,0))
    //println("Reflexive path passed!")
  }

}
