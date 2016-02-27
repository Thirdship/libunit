package com.thirdship.libunit

/** A conversion implementation for creating a unit-conversion graph, and A* algorithm graph search implementation
  *
  * @note The following implements units and conversions in terms of graph theory. Units are vertices and conversions are edges.
  *       Each conversion carries with it the factor of conversion, such that 'start'*'factor'='end'.
  *       Each conversion also has a cost associated with the conversion, referring to the loss of precision when doing the conversion.
  *
  */
object TSUnitConversion {

  var allConversions = List.empty[ConversionEdge]
  var allTSUnits = List.empty[TSUnit]

  /**
    * Returns a conversion, if it exists, between the given units.
    *
    * @param    start  the unit to convert from
    * @param    end    the unit to convert to
    * @return   a reflexive, self-referential edge that converts a unit to itself, if desired
    *           or, a conversion as cached if such exists
    *           or, a conversion that is inverted if a conversion defined as converting from 'end' and to 'start' exists
    *           otherwise, returns a null conversion
    */

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

  /**
    * Returns a list of units adjacent to the the given unit in the unit-conversion graph
    *
    * @note "Adjacent" refers to whether or not a conversion exists between two given units
    *
    * @param    center the unit about which adjacent units are found
    * @return   a list of adjacent units to center
    */

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

  /**
    * Returns a value of approximate cost that is used to evaluate best paths
    *
    * @param  current The unit start
    * @param  goal    The unit goal
    * @return a value that is at most the actual cost converting form current to goal
    */
  def heuristic(current: TSUnit, goal: TSUnit): Double = {
    val piece: ConversionEdge = searchConversions(current,goal)
    if(piece != null)
      piece.cost // returns cost if already calculated
    else
      0 //Dijkstra's Algorithm
  }

  /**
    * Creates a new conversion from start to goal based on the cameFrom map. This is the last step of the A* algorithm.
    * Starting from the goal unit, cameFrom returns the unit previously found in the path A* constructed.
    * Once cameFrom reaches the start node, a conversion is constructed form start to goal of the product of all the conversion factors in between, and the sum of all the costs in between.
    *
    * @param  cameFrom  A map with keys of units and values of the unit they "came from" during the A* algorithm search
    * @param  start     The starting unit from the search
    * @param  goal      The ending unit form the search
    * @return A conversion from start to goal, using the information from cameFrom
    */
  def reconstructPath(cameFrom: Map[ TSUnit, TSUnit], start: TSUnit, goal: TSUnit): ConversionEdge = {
    var totalPath = List.empty[TSUnit]
    var node: TSUnit = goal.asInstanceOf[TSUnit]
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

  /**
    * A pathmaxx algorithm that maintains consistency in heuristic cost reporting.
    * This ensures that the heuristic from current to goal reproted to the algorithm is always no more than
    * the heuristic from a neighbor of current to the goal and the cost from converting from current to that neighbor.
    *
    * @param  current the current unit being explored
    * @param  goal    the goal unit at the end of the heuristic
    * @return the consistent cost of converting from current to goal
    */
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

  /**
    * The A* graph search algorithm. While many implementations exist in other forms, this one is unique to our purposes of converting between units.
    * The algorithm functions as a way to find the least-costly path of conversion from one unit to another.
    * The idea is that the cost to go from the start unit to the goal unit through any other given current unit will be the cost from the start to current plus the heuristic cost from the current to the goal.
    * This idea is represented by the simple equation f = g + h. The trick is to find the smallest f-score.
    * In this implementation, values for f are stored in the fScores map, keyed to the specified unit.
    * Values for g are stored in the gScores map, keyed by the specified unit.
    *
    * closedSet is the set of units in the graph that are not the goal. openSet is the set of units that might be the goal.
    * The openSet, then, represents the fringes of the search, whereas the closedSet represents the insides of the searched map.
    *
    * cameFrom is a map of units to units, storing the value of the unit was visited right before the unit key.
    * This is used to reconstruct the path taken by the algorithm.
    *
    * @param start  The start unit for the search, the one being converted from.
    * @param goal   The goal unit for the search, the one being converted to.
    * @return A conversion from start to goal that is the most conversion-cost-efficient.
    */
  def aStar(start: TSUnit, goal: TSUnit): ConversionEdge = {
    //println("A* algorithm begun! " + start + " is the start and " + goal + " is the goal.")
    var closedSet = List.empty[TSUnit]
    var openSet = List.empty[TSUnit]
    openSet :+= start
    var cameFrom = Map.empty[TSUnit,TSUnit]
    allTSUnits.foreach(unit => {
      cameFrom += (unit -> null)
    })
    // All the units populate cameFrom as keys, and are adjusted as the algorithm finds paths.
    var current: TSUnit = null

    var gScore = Map.empty[TSUnit,Double]
    var fScore = Map.empty[TSUnit,Double]
    allTSUnits.foreach(unit => {
      if(unit == start){
        gScore += start -> 0
        fScore += start -> heuristicPM(start,goal)
        // If the specified unit is the start, then the cost from start to start is 0, and the cost from start to goal is the heuristic only.
      }
      else{
        gScore += unit -> Double.PositiveInfinity
        fScore += unit -> Double.PositiveInfinity
        // By initializing every other unit with large scores, the algorithm does not investigate these until necessary.
      }
    })

    while(openSet.nonEmpty){
      current = openSet.reduce((x,y) =>{
        if(fScore(x) < fScore(y))
          x
        else y
      })
      // current is set to be the unit in the openSet iwht the lowest f score
      //println(current + " is the current unit (A*)")
      if(current == goal){
        val shortcut = reconstructPath(cameFrom,start,goal)
        return shortcut
      } // If the unit with the lowest f score is the goal, then the path has been found.
      //println(current + " is not the goal (A*)")
      openSet = openSet.filterNot(_ == current)
      closedSet :+= current
      // Otherwise, since the current unit is not the goal, it is removed from openSet and added to closedSet before proceeding.
      val neighbors = searchNeighbors(current).filterNot(neighbor => closedSet.contains(neighbor))
      // neighbors is a list of all units adjacent to current not already eliminated. We now look for a possible path to proceed forward on.
      neighbors.foreach(neighbor => {
        //println(neighbor + " is a neighbor of " + current + " (A*)")
        val maybeGScore = gScore(current) + heuristicPM(current,neighbor)
        var notBetterPath = false
        // A flag to see if the path being tried is better than existing paths to the neighbor.
        if(!openSet.contains(neighbor)){
          //println("Adding " + neighbor + " to the open set (A*)")
          openSet :+= neighbor
        } // No existing paths are present.
        else if(maybeGScore >= gScore(neighbor)){
          notBetterPath = true
        } // The path already found to get to neighbor is better, so move to the next neighbor.
        if(!notBetterPath){
          // Whether the path found is better than existing paths, or no others exist, we store it.
          cameFrom -= neighbor
          cameFrom += (neighbor -> current)
          //println(neighbor + " came from " + current + " (A*)")
          // Set neighbor to have come from the current unit.
          gScore -= neighbor
          gScore += (neighbor -> maybeGScore)
          // Set the g score of neighbor to have gone through this current unit.
          fScore -= neighbor
          fScore += (neighbor -> (gScore(neighbor) + heuristicPM(neighbor,goal)))
          // Set the f score of neighbor to have gone through this current unit.
        }
      }) // Repeat over all neighbors of the current unit.
    } // Repeat over possible current units until no more units exist in openSet
    val failure: ConversionEdge = new ConversionEdge(new BaseTSUnit("Failed A*"),new BaseTSUnit("Failed A*"),1,0)
    failure
    // The goal unit was never found, or otherwise the algorithm failed.
  }
}

/**
  * A class of conversions, implemented as edges in the unit-conversion graph.
  * These edges are treated as being directed, carrying both the conversion factor and the cost of converting.
  *
  * @param start  The unit being converted from.
  * @param end    The unit being converted to.
  * @param factor The conversion factor from start to end.
  * @param cost   A measure of the precision cost form using this conversion from start to end.
  */
case class ConversionEdge(start: TSUnit, end: TSUnit, factor: Double, cost: Double)

