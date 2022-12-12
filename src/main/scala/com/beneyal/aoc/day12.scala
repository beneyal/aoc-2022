package com.beneyal.aoc

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object day12:
  type Point = (Int, Int)
  type Graph = Map[Point, Vector[Point]]

  def readInput() =
    val heightMap = Source.fromResource("day12-input.txt").getLines().toArray.map(_.toArray)
    val startCol  = heightMap.indexWhere(_.contains('S'))
    val startRow  = heightMap(startCol).indexWhere(_ == 'S')
    val endCol    = heightMap.indexWhere(_.contains('E'))
    val endRow    = heightMap(endCol).indexWhere(_ == 'E')
    heightMap(startCol)(startRow) = 'a'
    heightMap(endCol)(endRow) = 'z'
    val graph =
      (for
        i        <- heightMap.indices
        j        <- heightMap(i).indices
        (ni, nj) <- Vector((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1))
        if heightMap.indices.contains(ni) && heightMap(ni).indices.contains(nj) &&
          (Set(0, 1)(heightMap(ni)(nj) - heightMap(i)(j)) || heightMap(i)(j) >= heightMap(ni)(nj))
      yield (i, j) -> (ni, nj)).groupBy(_._1).view.mapValues(_.toVector.map(_._2)).toMap
    (heightMap, graph, (startCol, startRow), (endCol, endRow))

  def findEndPoint(graph: Graph, start: Point, target: Point): List[Point] =
    @tailrec
    def backtrace(parent: Map[Point, Point], current: Point, path: List[Point]): List[Point] =
      if current == start then path
      else backtrace(parent, parent(current), current :: path)

    @tailrec
    def loop(q: Queue[Point], parent: Map[Point, Point], seen: Set[Point]): List[Point] =
      if q.isEmpty then List.empty
      else
        val (p @ (i, j), q0) = q.dequeue
        if p == target then backtrace(parent, target, List.empty)
        else
          val nextPossible = graph(p).filterNot(parent.contains)
          loop(q0.enqueueAll(nextPossible), parent ++ nextPossible.map(_ -> p), seen + p)

    loop(Queue(start), Map(start -> (-1, -1)), Set.empty)

  def main(args: Array[String]): Unit =
    val (map, graph, startPos, endPos) = readInput()
    val allAs                          = graph.keys.filter { case (i, j) => map(i)(j) == 'a' }
    println(s"Solution for Part 1: ${findEndPoint(graph, startPos, endPos).size}")
    println(s"Solution for Part 2: ${allAs.map(findEndPoint(graph, _, endPos)).filter(_.nonEmpty).map(_.size).min}")
