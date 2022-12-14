package com.beneyal.aoc

import scala.io.Source
import scala.annotation.tailrec

object day14:
  import Square.*

  enum Square:
    case Rock, Air, Sand

  final case class Point(x: Int, y: Int)
  final case class Instruction(start: Point, end: Point)
  type Cave = Map[Point, Square]

  extension (p: Point)
    def to(p2: Point): List[Point] =
      val Point(x1, y1) = p
      val Point(x2, y2) = p2
      if x1 == x2
      then (math.min(y1, y2) to math.max(y1, y2)).toList.map(Point(x1, _))
      else (math.min(x1, x2) to math.max(x1, x2)).toList.map(Point(_, y1))

  def readInput() =
    Source
      .fromResource("day14-input.txt")
      .getLines()
      .flatMap { s =>
        s.split(" -> ").map { case s"$x,$y" => Point(x.toInt, y.toInt) }.sliding(2).map { case Array(p1, p2) =>
          Instruction(p1, p2)
        }
      }
      .toList

  def constructCave(instructions: List[Instruction], infiniteX: Boolean = false): Cave =
    val withRocks = instructions.foldLeft(Map(Point(500, 0) -> Air)) { case (acc, Instruction(start, end)) =>
      acc ++ (start to end).map(_ -> Rock)
    }
    val pad          = 1000
    val xs           = withRocks.keySet.map(_.x)
    val ys           = withRocks.keySet.map(_.y)
    val (minX, maxX) = if infiniteX then (xs.min - pad, xs.max + pad) else (xs.min, xs.max)
    val (minY, maxY) = (ys.min, ys.max)
    val cave =
      withRocks ++ (for
        x <- minX to maxX
        y <- minY to maxY
        p = Point(x, y)
        if !withRocks.contains(p)
      yield p -> Air).toMap
    if infiniteX then cave ++ (minX to maxX).flatMap(x => List(Point(x, maxY + 1) -> Air, Point(x, maxY + 2) -> Rock))
    else cave

  def countHowManyComeToRest(cave: Cave, source: Point): Int =
    @tailrec
    def loop(currentSand: Point, cave: Cave, result: Int): Int =
      val Point(x, y) = currentSand
      val down        = cave.get(Point(x, y + 1))
      val diagLeft    = cave.get(Point(x - 1, y + 1))
      val diagRight   = cave.get(Point(x + 1, y + 1))
      cave(currentSand) match
        case Air =>
          (down, diagLeft, diagRight) match
            case (Some(Air), _, _)             => loop(Point(x, y + 1), cave, result)
            case (Some(_), Some(Air), _)       => loop(Point(x - 1, y + 1), cave, result)
            case (Some(_), Some(_), Some(Air)) => loop(Point(x + 1, y + 1), cave, result)
            case (Some(_), Some(_), Some(_))   => loop(source, cave.updated(currentSand, Sand), result + 1)
            case _                             => result
        case _ => result

    loop(source, cave, 0)

  def main(args: Array[String]): Unit =
    val input  = readInput()
    val source = Point(500, 0)
    println(s"Solution for Part 1: ${countHowManyComeToRest(constructCave(input, infiniteX = false), source)}")
    println(s"Solution for Part 2: ${countHowManyComeToRest(constructCave(input, infiniteX = true), source)}")
