package com.beneyal.aoc

import scala.io.Source

object day3:
  object part1:
    type Compartment = Set[Char]

    type Rucksack = (Compartment, Compartment)

    object Rucksack:
      def fromString(s: String): Rucksack =
        val first = Set.from(s.take(s.length / 2))
        val second = Set.from(s.drop(s.length / 2))
        (first, second)

  object part2:
    type Rucksack = Set[Char]

    object Rucksack:
      def fromString(s: String): Rucksack = Set.from(s)

  def readInput(): Vector[String] =
    Source.fromResource("day3-input.txt").getLines().toVector

  def getPriority(c: Char): Int =
    if c.isLower then c - 96 else c - 38

  def main(args: Array[String]): Unit =
    val part1Result = readInput()
      .map(part1.Rucksack.fromString)
      .map(_ intersect _)
      .map(_.map(getPriority).head)
      .sum
    println(s"Solution for Part 1: $part1Result")

    val part2Result = readInput()
      .map(part2.Rucksack.fromString)
      .map(_.map(getPriority))
      .grouped(3)
      .collect { case Vector(r1, r2, r3) => r1 & r2 & r3 }
      .map(_.head)
      .sum
    println(s"Solution for Part 2: $part2Result")
