package com.beneyal.aoc

import scala.io.Source

object day01:
  def readInput(): Array[Array[Int]] =
    Source
      .fromResource("day1-input.txt")
      .getLines()
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").map(_.toInt))

  def getMaxCalories(caloriesArray: Array[Array[Int]]): Int =
    caloriesArray.map(_.sum).max

  def getTopThreeSum(caloriesArray: Array[Array[Int]]): Int =
    caloriesArray.map(_.sum).sorted(using Ordering[Int].reverse).slice(0, 3).sum

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${getMaxCalories(input)}")
    println(s"Solution for Part 2: ${getTopThreeSum(input)}")
