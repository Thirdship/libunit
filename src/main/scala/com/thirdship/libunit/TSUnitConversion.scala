package com.thirdship.libunit

/**
  * Created by blaze on 1/22/16.
  */
object TSUnitConversion {

  var allConversions = List.empty[ConversionEdge]
  var allTSUnits = List.empty[TSUnit]

  def searchConversions(start: TSUnit, end: TSUnit): ConversionEdge = {
    if(start == end)
      return new ConversionEdge(start, start, 1, 0)
    allConversions.foreach(conversion => {
      if(conversion.start == start && conversion.end == end)
        return conversion
      else if(conversion.end == start && conversion.start == end)
        return new ConversionEdge(start,end,1/conversion.factor,1+conversion.cost)
    })
    null
  }

  def searchNeighbors(center: TSUnit): List[TSUnit] = {
    var neighbors = List.empty[TSUnit]
    //println(center + " is my center (SN)")
    allTSUnits.foreach(maybeNeighbor => {
      if(searchConversions(center,maybeNeighbor) != null && center != maybeNeighbor){
        neighbors :+= maybeNeighbor
        //println(maybeNeighbor + " is a neighbor of " + center + " (SN)")
      }
    })
    neighbors
  }

  def heuristic(current: TSUnit, goal: TSUnit): Double = {
    val piece: ConversionEdge = searchConversions(current,goal)
    if(piece != null)
      piece.cost // returns cost if already calculated
    else
      0 //Dijkstra's Algorithm
  }

  def reconstructPath(cameFrom: Map[ TSUnit, TSUnit], current: TSUnit, start: TSUnit, goal: TSUnit): ConversionEdge = {
    var totalPath = List.empty[TSUnit]
    var node: TSUnit = current.asInstanceOf[TSUnit]
    while(node != null){
      totalPath +:= node
      node = cameFrom(node)
    }
    //println(totalPath)
    var shortcutFactor: Double = 1
    var shortcutCost: Double = 0
    for (k <- 0 until totalPath.length-1){
      val piece = searchConversions(totalPath(k),totalPath(k+1))
      shortcutFactor *= piece.factor
      shortcutCost += piece.cost
    }
    val shortcut = new ConversionEdge(start,goal,shortcutFactor,shortcutCost)
    allConversions :+= shortcut
    //println("Path reconstructed! " + shortcut)
    shortcut
  }

  def heuristicPM(current: TSUnit, goal: TSUnit): Double = {
    var neighborCost = List.empty[Double]
    var conversion: ConversionEdge = null
    neighborCost :+= heuristic(current,goal)
    allTSUnits.foreach(unit => {
      conversion = searchConversions(current,unit)
      if(conversion != null && unit != current)
        neighborCost :+= heuristic(unit,goal) - conversion.cost
    })
    neighborCost.max
  }

  def aStar(start: TSUnit, goal: TSUnit): ConversionEdge = {
    //println("A* algorithm begun! " + start + " is the start and " + goal + " is the goal.")
    var closedSet = List.empty[TSUnit]
    var openSet = List.empty[TSUnit]
    openSet :+= start
    var cameFrom = Map.empty[TSUnit,TSUnit]
    allTSUnits.foreach(unit => {
      cameFrom += (unit -> null)
    })
    var current: TSUnit = null

    var gScore = Map.empty[TSUnit,Double]
    var fScore = Map.empty[TSUnit,Double]
    allTSUnits.foreach(unit => {
      if(unit == start){
        gScore += start -> 0
        fScore += start -> heuristicPM(start,goal)
      }
      else{
        gScore += unit -> Double.PositiveInfinity
        fScore += unit -> Double.PositiveInfinity
      }
    })

    while(openSet.nonEmpty){
      current = openSet.reduce((x,y) =>{
        if(fScore(x) < fScore(y))
          x
        else y
      })
      //println(current + " is the current unit (A*)")
      if(current == goal){
        val shortcut = reconstructPath(cameFrom,current,start,goal)
        return shortcut
      }
      //println(current + " is not the goal (A*)")
      openSet = openSet.filterNot(_ == current)
      closedSet :+= current
      val neighbors = searchNeighbors(current).filterNot(neighbor => closedSet.contains(neighbor))
      neighbors.foreach(neighbor => {
        //println(neighbor + " is a neighbor of " + current + " (A*)")
        val maybeGScore = gScore(current) + heuristicPM(current,neighbor)
        var notBetterPath = false
        if(!openSet.contains(neighbor)){
          //println("Adding " + neighbor + " to the open set (A*)")
          openSet :+= neighbor
        }
        else if(maybeGScore >= gScore(neighbor)){
          notBetterPath = true
        }
        if(!notBetterPath){
          cameFrom -= neighbor
          cameFrom += (neighbor -> current)
          //println(neighbor + " came from " + current + " (A*)")
          gScore -= neighbor
          gScore += (neighbor -> maybeGScore)
          fScore -= neighbor
          fScore += (neighbor -> (gScore(neighbor) + heuristicPM(neighbor,goal)))
        }
      })
    }
    val failure: ConversionEdge = new ConversionEdge(new BaseTSUnit("Failed A*"),new BaseTSUnit("Failed A*"),1,0)
    failure
  }
}

case class ConversionEdge(start: TSUnit, end: TSUnit, factor: Double, cost: Double)

