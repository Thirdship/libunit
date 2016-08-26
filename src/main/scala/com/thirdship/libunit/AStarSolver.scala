package com.thirdship.libunit

/** A conversion implementation for creating a unit-conversion graph, and A* algorithm graph search implementation
  *
  * @note The following implements units and conversions in terms of graph theory. Units are vertices and conversions are edges.
  *       Each conversion carries with it the factor of conversion, such that 'start'*'factor'='end'.
  *       Each conversion also has a cost associated with the conversion, referring to the loss of precision when doing the conversion.
  *
  */
case class AStarSolver(var allTSUnits: List[String], var allConversions: List[ConversionEdge[String, Double, Double]]) {

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

  def getConversions(start: String, end: String): Option[ConversionEdge[String, Double, Double]] = {
    if (start == end) {
      return Some(new ScalarConversionEdge(start, start, 1))} // Reflexive edge
    val equiv = allConversions.filter(a => (a.start == start) && (a.end == end))
    if (equiv.nonEmpty) {
      return equiv.headOption} // Cached edge
    val invert = allConversions.filter(a => (a.end == start) && (a.start == end))
    if (invert.nonEmpty) {
      return Some(invert.head.inverted)} // Commutative edge
    None // Null edge
  }

  /**
    * Returns a list of units adjacent to the the given unit in the unit-conversion graph
    *
    * @note "Adjacent" refers to whether or not a conversion exists between two given units
    * @param    center the unit about which adjacent units are found
    * @return   a list of adjacent units to center
    */

  def getNeighbors(center: String): List[String] = {
    var neighbors = List.empty[String]
    allTSUnits.foreach(maybeNeighbor => {
      if (getConversions(center, maybeNeighbor).isDefined && center != maybeNeighbor) {
        neighbors :+= maybeNeighbor}
    })
    neighbors
  }

  /**
    * Returns a value of approximate cost that is used to evaluate best paths.
    *
    * @note   The if-else structure is used for modularity, in order to add in different heuristics at a later time.
    * @param  start The unit start
    * @param  end   The unit goal
    * @return a value that is at most the actual cost converting form current to goal
    */
  def heuristic(start: String, end: String): Double = {
    val piece = getConversions(start, end)
    if (piece.isDefined) {
      piece.get.cost} // returns cost if already calculated
    else {
      0} // Dijkstra's Algorithm
  }

  /**
    * Creates a new conversion from start to goal based on the cameFrom map. This is the last step of the A* algorithm.
    * Starting from the goal unit, cameFrom returns the unit previously found in the path A* constructed.
    * Once cameFrom reaches the start node, a conversion is constructed from start to end of the product of
    *   all the conversion factors, and the sum of all the costs.
    *
    * @param  cameFrom  A map with keys of units and values of the unit they "came from" during the A* algorithm search
    * @param  start     The starting unit from the search
    * @param  end      The ending unit form the search
    * @return A conversion from start to goal, using the information from cameFrom
    */
  def reconstructPath(cameFrom: Map[String, String], start: String, end: String): ConversionEdge[String, Double, Double] = {
    val path = reconstructConversionEdgePathList(cameFrom, end)

    val conversionTo = path.map(ce => ce.conversion.to).foldLeft((a: Double) => a)((chain, func) => func.compose(chain))
    val conversionFrom = path.map(ce => ce.conversion.from).foldLeft((a: Double) => a)((chain, func) => chain.compose(func))
    val cost = path.map(ce => ce.cost).sum
    val conversion = Conversion(conversionTo, conversionFrom)

    // Save result
    val shortcut = ConversionEdge(start, end, conversion, cost)
    allConversions :+= shortcut
    shortcut
  }

	/**
      * Create a list of conversions that the code traveled to in order to create the end conversion
      * @param  cameFrom  A map with keys of units and values of the unit they "came from" during the A* algorithm search
      * @param  end      The ending unit form the search
      * @return A list of conversion edges that when listed in order describe the path of conversion from start to end
	  */
  private def reconstructConversionEdgePathList(cameFrom: Map[String, String], end: String): List[ConversionEdge[String, Double, Double]] = {
    var list = List.empty[String]
    var currentUnit = end
    list = list.::(currentUnit)

    if(cameFrom.get(currentUnit).isEmpty) {
      return List(getConversions(end, end).get)
    }

    while(cameFrom.get(currentUnit).isDefined) {
      currentUnit = cameFrom(currentUnit)
      list = list.::(currentUnit)
    }

    val conversions = list.sliding(2, 1).flatMap(path => {
      getConversions(path.head, path(1))
    }).toList

    conversions
  }

  /**
    * A path-max algorithm that maintains consistency in heuristic cost reporting.
    * This ensures that the heuristic from current to goal reported to the algorithm is always no more than
    * the heuristic from a neighbor of current to the goal and the cost from converting from current to that neighbor.
    *
    * @param  start  the current unit being explored
    * @param  end    the goal unit at the end of the heuristic
    * @return the consistent cost of converting from current to goal
    */
  def heuristicPM(start: String, end: String): Double = {
    var neighborCost = List.empty[Double]
    var conversion: Option[ConversionEdge[String, Double, Double]] = None
    neighborCost :+= heuristic(start, end)
    allTSUnits.foreach(unit => {
      conversion = getConversions(start, unit)
      if(conversion.isDefined && unit != start) {
        neighborCost :+= heuristic(unit, end) - conversion.get.cost}
    })
    neighborCost.max
  }

  /**
    * The A* graph search algorithm. While many implementations exist in other forms, this one is unique to our purposes
    *   of converting between units. The algorithm functions as a way to find the least-costly path of conversion from
    *   one unit to another. The idea is that the cost to go from the start unit to the goal unit through any other
    *   given current unit will be the cost from the start to current plus the heuristic cost from the current to the
    *   goal. This idea is represented by the simple equation f = g + h. The trick is to find the smallest f-score.
    *   In this implementation, values for f are stored in the fScores map, keyed to the specified unit. Values for g
    *   are stored in the gScores map, keyed by the specified unit.
    *
    * closedSet is the set of units in the graph that are not the goal. openSet is the set of units that might be the
    *   goal. The openSet, then, represents the fringes of the search, whereas the closedSet represents the insides of
    *   the searched map. The openSet is iterated over until the goal is found, and populated by neighbors of units in
    *   the openSet.
    *
    * cameFrom is a map of units to units, storing the value of the unit was visited right before the unit key in the
    *   final path. This is used to reconstruct the path taken by the algorithm. As different ways of traveling from one
    *   unit to another are found, the path that has the lowest cost is kept, and the higher-cost path is thrown out.
    *
    * @param start  The start unit for the search, the one being converted from.
    * @param end   The goal unit for the search, the one being converted to.
    * @return A conversion from start to goal that is the most conversion-cost-efficient.
    */
  def solve(start: String, end: String): ConversionEdge[String, Double, Double] = {
    if (! allTSUnits.contains(start)) {
      val startNotUnit = new ScalarConversionEdge("start is", "not a unit!", 1) // TODO replace with exceptions or None
      return startNotUnit}
    if (! allTSUnits.contains(end)) {
      val endNotUnit = new ScalarConversionEdge("end is", "not a unit!", 1) // TODO replace with exceptions or None
      return endNotUnit}
    var closedSet = List.empty[String]
    var openSet = List.empty[String]
    openSet :+= start
    var cameFrom = Map.empty[String, String]
    var current: String = ""

    var gScore = Map.empty[String, Double]
    var fScore = Map.empty[String, Double]
    gScore += start -> 0
    fScore += start -> heuristicPM(start, end)

    while(openSet.nonEmpty) {
      openSet.sortWith(fScore.getOrElse(_, Double.PositiveInfinity) < fScore.getOrElse(_, Double.PositiveInfinity))
      current = openSet.head
      if(current == end) {
        val shortcut = reconstructPath(cameFrom, start, end)
        return shortcut}
      openSet = openSet.filterNot(_ == current)
      closedSet :+= current
      val neighbors = getNeighbors(current).filterNot(neighbor => closedSet.contains(neighbor))
      neighbors.foreach(neighbor => {
        val maybeGScore = gScore(current) + getConversions(current, neighbor).get.cost
        var notBetterPath = false
        if (! openSet.contains(neighbor)) {
          openSet :+= neighbor}
        else if (maybeGScore >= gScore.getOrElse(neighbor, Double.PositiveInfinity)) {
          notBetterPath = true}
        if (! notBetterPath) {
          cameFrom -= neighbor
          cameFrom += (neighbor -> current)
          gScore -= neighbor
          gScore += (neighbor -> maybeGScore)
          fScore -= neighbor
          fScore += (neighbor -> (gScore(neighbor) + heuristicPM(neighbor, end)))
        }
      })
    }

    // TODO replace with exception or None
    val failure = new ScalarConversionEdge("Failed A*", "Failed A*", 1)
    failure
  }
}
