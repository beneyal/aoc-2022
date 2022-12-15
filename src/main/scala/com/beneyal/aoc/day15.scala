package com.beneyal.aoc

import scala.io.Source
import scala.annotation.tailrec

object day15:
  final case class Point(x: Int, y: Int)
  final case class SensorBeaconData(
      sensor: Point,
      beacon: Point,
      distance: Int,
      inRange: Point => Boolean,
      skip: Point => Int
  )

  def readInput(): List[SensorBeaconData] =
    Source
      .fromResource("day15-input.txt")
      .getLines()
      .map { case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        val s = Point(sx.toInt, sy.toInt)
        val b = Point(bx.toInt, by.toInt)
        val d = math.abs(s.x - b.x) + math.abs(s.y - b.y)

        def getRemainingDistance(p: Point): Int =
          d - math.abs(s.y - p.y)

        def inRange(p: Point): Boolean =
          val d0 = getRemainingDistance(p)
          p.x >= s.x - d0 && p.x <= s.x + d0

        def skip(p: Point): Int =
          if inRange(p)
          then s.x + getRemainingDistance(p) + 1
          else p.x

        SensorBeaconData(s, b, d, inRange, skip)
      }
      .toList

  def makeInRangeFunction(data: List[SensorBeaconData]): Point => Boolean =
    data.foldLeft((_: Point) => false) { case (acc, SensorBeaconData(_, b, _, f, _)) =>
      p => p != b && (acc(p) || f(p))
    }

  def countInRange(inRange: Point => Boolean, xs: List[Int], y: Int) =
    xs.count(x => inRange(Point(x, y)))

  def getTuningFrequency(data: List[SensorBeaconData], upperBound: Int): Long =
    @tailrec
    def loop(x: Int, y: Int): Long =
      if x >= upperBound then loop(0, y + 1)
      else
        val skips = data.map(_.skip(Point(x, y))).filter(_ != x)
        if skips.isEmpty then x.toLong * 4000000 + y.toLong
        else loop(skips.head, y)

    loop(0, 0)

  def main(args: Array[String]): Unit =
    val input        = readInput()
    val maxD         = input.map(_.distance).max
    val xs           = input.flatMap(p => List(p.sensor, p.beacon)).map(_.x)
    val (minX, maxX) = (xs.min, xs.max)
    val xRange       = (minX - maxD to maxX + maxD).toList
    val inRange      = makeInRangeFunction(input)
    println(s"Solution for Part 1: ${countInRange(inRange, xRange, 2000000)}")
    println(s"Solution for Part 2: ${getTuningFrequency(input, 4000000)}")
