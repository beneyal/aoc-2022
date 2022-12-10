package com.beneyal.aoc

import scala.io.Source
import scala.annotation.tailrec

object day09:
  import Direction.*

  enum Direction:
    case Up, Right, Down, Left

  def readInput(): List[Direction] =
    Source
      .fromResource("day9-input.txt")
      .getLines()
      .toList
      .flatMap { case s"$dir $count" =>
        dir match
          case "U" => Vector.fill(count.toInt)(Up)
          case "R" => Vector.fill(count.toInt)(Right)
          case "D" => Vector.fill(count.toInt)(Down)
          case "L" => Vector.fill(count.toInt)(Left)
      }

  extension (p1: (Int, Int))
    def notTooFarFrom(p2: (Int, Int)): Boolean =
      math.abs(p1._1 - p2._1) <= 1 && math.abs(p1._2 - p2._2) <= 1
    def moveTowards(p2: (Int, Int)): (Int, Int) =
      (p1, p2) match
        case ((x1, y1), (x2, y2)) if x2 > x1 && y2 > y1 => (x1 + 1, y1 + 1)
        case ((x1, y1), (x2, y2)) if x2 > x1 && y2 < y1 => (x1 + 1, y1 - 1)
        case ((x1, y1), (x2, y2)) if x2 < x1 && y2 < y1 => (x1 - 1, y1 - 1)
        case ((x1, y1), (x2, y2)) if x2 < x1 && y2 > y1 => (x1 - 1, y1 + 1)
        case ((x1, y1), (x2, y2)) if x2 < x1            => (x1 - 1, y1)
        case ((x1, y1), (x2, y2)) if x2 > x1            => (x1 + 1, y1)
        case ((x1, y1), (x2, y2)) if y2 > y1            => (x1, y1 + 1)
        case ((x1, y1), (x2, y2)) if y2 < y1            => (x1, y1 - 1)
        case _                                          => ???

  def getHeadPositions(directions: List[Direction]): Vector[(Int, Int)] =
    directions.foldLeft(Vector((0, 0))) { case (acc, dir) =>
      acc :+ (dir match
        case Up    => (acc.last._1, acc.last._2 + 1)
        case Right => (acc.last._1 + 1, acc.last._2)
        case Down  => (acc.last._1, acc.last._2 - 1)
        case Left  => (acc.last._1 - 1, acc.last._2)
      )
    }

  def getUniqueKnotPositionCount(directions: List[Direction], knot: Int): Int =
    @tailrec
    def loop(headPositions: List[(Int, Int)], tailPos: (Int, Int), path: List[(Int, Int)]): List[(Int, Int)] =
      headPositions match
        case prev :: current :: rest =>
          if tailPos notTooFarFrom current
          then loop(current :: rest, tailPos, tailPos :: path)
          else
            val newTail = tailPos.moveTowards(current)
            loop(current :: rest, newTail, newTail :: path)
        case _ => path.reverse

    val headPositions = getHeadPositions(directions).toList
    (0 until knot)
      .foldLeft(headPositions) { case (acc, _) =>
        loop(acc, (0, 0), List((0, 0)))
      }
      .toSet
      .size

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${getUniqueKnotPositionCount(input, 1)}")
    println(s"Solution for Part 2: ${getUniqueKnotPositionCount(input, 9)}")
