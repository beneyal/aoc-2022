package com.beneyal.aoc

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object day16:
  type WeightedGraph = Map[Valve, Set[(Valve, Int)]]

  final case class Valve(name: String, pressure: Int)

  final case class QueueState(valve: Valve, time: Int, sum: Int, path: List[(Valve, Int)])

  /** Case class representing a complete traversal in the graph
    *
    * @param timeLimit
    *   The time limit for the traversal
    * @param time
    *   Minute at which the traversal ended
    * @param sum
    *   Maximum pressure collected during traversal
    * @param end
    *   Vertex at which the traversal ended
    * @param path
    *   Path from AA to `end`, excluding `end`
    */
  final case class TunnelTraversal(timeLimit: Int, time: Int, sum: Int, end: Valve, path: List[(Valve, Int)]):
    /** Summarize two traversals, namely human's and elephant's, to the sum of the overall pressures
      *
      * @param that
      *   Other traversal
      * @return
      *   The combined sum of pressures of the two traversals
      */
    def +(that: TunnelTraversal): Int =
      def pressureScan(tt: TunnelTraversal): List[Int] =
        val pathMinusLast = tt.path.reverse
          .foldLeft((List.empty[Int], 0)) { case ((acc, s), (v, w)) =>
            (acc ++ List.fill(w)(s + v.pressure), s + v.pressure)
          }
          ._1
        pathMinusLast ++ List.fill(tt.timeLimit - tt.time)(tt.sum)

      val thisScan = pressureScan(this)
      val thatScan = pressureScan(that)
      thisScan.zip(thatScan).map(_ + _).sum

    def pressure: Int =
      val pathTotal = path.reverse
        .foldLeft((0, 0)) { case ((acc, s), (v, w)) =>
          (acc + (s + v.pressure) * w, s + v.pressure)
        }
        ._1
      pathTotal + (timeLimit - time) * sum

  /** Given the initial graph in the input, eliminate zero-pressure vertices and create a complete graph between all
    * non-zero vertices. The weights between vertices are the number of steps + 1 (which is the extra minute for opening
    * a valve)
    *
    * @param graph
    *   The parsed input graph
    * @param source
    *   The source vertex, namely "AA"
    * @return
    *   A complete graph without zero-pressure nodes
    */
  def minimizeGraph(graph: WeightedGraph, source: Valve): WeightedGraph =
    /** Breadth-first traversal of a graph that skips zero-pressure vertices.
      *
      * @param q
      *   Queue used for the traversal
      * @param seen
      *   Vertices that have been seen during traversal
      * @param result
      *   Accumulator of pairs (vertex, distance) from the vertex that the queue starts with
      * @return
      *   Set of pairs (vertex, distance)
      */
    @tailrec
    def bfs(q: Queue[(Valve, Int)], seen: Set[Valve], result: Set[(Valve, Int)]): Set[(Valve, Int)] =
      if q.isEmpty then result
      else
        val ((v, d), q0) = q.dequeue
        val neighbors    = graph(v).filterNot { case (u, _) => seen(u) }.map { case (u, _) => u -> (d + 1) }
        if v.pressure == 0 then bfs(q0.enqueueAll(neighbors), seen + v, result)
        else bfs(q0.enqueueAll(neighbors), seen + v, result + (v -> d))

    graph
      // For every vertex in the graph, calculate new distances and neighbors...
      .map { case (k, _) =>
        k -> (bfs(Queue(k -> 1), Set.empty, Set.empty) - (k -> 1))
      }
      // ...and keep only the non-zero vertices, plus the source vertex
      .filterNot { case (v, _) => v.pressure == 0 && v != source }

  /** Parse input data.
    *
    * @return
    *   Pair of (graph, source vertex)
    */
  def readInput(): (WeightedGraph, Valve) =
    val valves = Source
      .fromResource("day16-input.txt")
      .getLines()
      .map {
        case s"Valve $v has flow rate=$r; tunnels lead to valves $vs" =>
          Valve(v, r.toInt) -> vs.split(", ").toSet
        case s"Valve $v has flow rate=$r; tunnel leads to valve $v0" =>
          Valve(v, r.toInt) -> Set(v0)
      }
      .toList
    val nameToValve = valves.map(_._1).map(v => v.name -> v).toMap
    val graph = valves.foldLeft(Map.empty[Valve, Set[(Valve, Int)]]) { case (acc, (v, vs)) =>
      acc + (v -> vs.map(nameToValve).map(_ -> 1))
    }
    graph -> nameToValve("AA")

  /** Calculate the maximum pressure possible given constraints.
    *
    * @param graph
    *   Input graph (assumed to be minimized using above function)
    * @param source
    *   Source vertex
    * @param withElephant
    *   Whether we have an elephant friend helping out
    * @return
    *   Maximum pressure possible
    */
  def getMaxPressure(graph: WeightedGraph, source: Valve, withElephant: Boolean = false): Int =
    val timeLimit = if withElephant then 26 else 30

    /** Find maximum pressure possible in the graph using breadth-first traversal
      *
      * @param q
      *   Queue for the breath-first traversal. Every element is a QueueState consisting of the same components as in
      *   `TunnelTraversal`, but represent an unfinished traversal
      * @param responsibilities
      *   Used if an elephant is on our side: which vertices should we focus on. If no elephant is present, this
      *   contains all vertices.
      * @param result
      *   Accumulator list of `TunnelTraversal` containing all possible traversals in the graph
      * @return
      *   All path traversals
      */
    @tailrec
    def loop(
        q: Queue[QueueState],
        responsibilities: Set[Valve],
        result: List[TunnelTraversal]
    ): List[TunnelTraversal] =
      if q.isEmpty then result
      else
        val (s, q0) = q.dequeue
        val v       = s.valve

        // Get all neighbors that reaching them is possible within the time limit and haven't been seen yet
        val neighbors = graph(v).filter { case (u, w) =>
          responsibilities(u) && (w + s.time) <= timeLimit && !s.path.map(_._1).contains(u)
        }

        // No more neighbors, we're done with this traversal
        if neighbors.isEmpty then
          val tt = TunnelTraversal(timeLimit, s.time, s.sum, v, s.path)
          loop(q0, responsibilities, tt :: result)
        else
          // Put all the neighbors in the queue with their updated state
          val newQueue = q0.enqueueAll(neighbors.map { case (u, w) =>
            QueueState(u, (s.time + w), u.pressure + s.sum, (v -> w) :: s.path)
          })

          loop(newQueue, responsibilities, result)

    val q                     = Queue(QueueState(source, 0, 0, List.empty))
    val verticesWithoutSource = graph.keySet - source

    if withElephant then
      // Collect assignments of vertices to human and elephant, take only half (as the other half is symmetrical)
      // and, as a heuristic, keep only assignments where the vertices are distributed evenly
      val assignments = verticesWithoutSource
        .subsets()
        .map(s => (s, verticesWithoutSource -- s))
        .takeWhile(_._1.size <= verticesWithoutSource.size / 2)
        .filter { case (s1, s2) => math.abs(s1.size - s2.size) <= 1 }

      // For each (human, elephant) assignments, take the best standalone traversal of each,
      // combine the traversals to get the best pressure for this pair, then take the maximum
      // of all options
      assignments.map { case (humanAssignment, elephantAssignment) =>
        val humanTraversals       = loop(q, humanAssignment, List.empty)
        val elephantTraversals    = loop(q, elephantAssignment, List.empty)
        val bestHumanTraversal    = humanTraversals.maxBy(_.pressure)
        val bestElephantTraversal = elephantTraversals.maxBy(_.pressure)
        bestHumanTraversal + bestElephantTraversal
      }.max
    else
      // No elephants around, make a standalone traversal on all vertices,
      // returning the maximum
      loop(q, graph.keySet, List.empty).map(_.pressure).max

  def main(args: Array[String]): Unit =
    val (graph, source) = readInput()
    val minimizedGraph  = minimizeGraph(graph, source)
    println(s"Solution for Part 1: ${getMaxPressure(minimizedGraph, source, withElephant = false)}")
    println(s"Solution for Part 2: ${getMaxPressure(minimizedGraph, source, withElephant = true)}")
