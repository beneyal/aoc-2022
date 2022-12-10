package com.beneyal.aoc

import scala.io.Source

object day04:
  def readInput(): Vector[(Set[Int], Set[Int])] =
    Source.fromResource("day4-input.txt").getLines().toVector.map { line =>
      val ranges     = line.split(',')
      val firstPair  = ranges(0).split('-').map(_.toInt)
      val secondPair = ranges(1).split('-').map(_.toInt)
      (
        (firstPair(0) to firstPair(1)).toSet,
        (secondPair(0) to secondPair(1)).toSet
      )
    }

  def main(args: Array[String]): Unit =
    val input = readInput()
    val part1Solution = input.filter { case (s1, s2) =>
      s1.subsetOf(s2) || s2.subsetOf(s1)
    }.size
    val part2Solution = input.filter { case (s1, s2) =>
      s1.intersect(s2).nonEmpty
    }.size
    println(s"Solution for Part 1: $part1Solution")
    println(s"Solution for Part 2: $part2Solution")
