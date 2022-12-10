package com.beneyal.aoc

import scala.io.Source

object day10:
  import Instruction.*

  enum Instruction:
    case Noop
    case AddX(v: Int)

  def readInput(): List[Instruction] =
    Source.fromResource("day10-input.txt").getLines().toList.flatMap {
      case "noop"     => List(Noop)
      case s"addx $v" => List(Noop, AddX(v.toInt))
    }

  def getXsUntilCycle(instructions: List[Instruction], cycle: Int): List[Int] =
    instructions.take(cycle - 1).scanLeft(1) { case (x, ins) =>
      ins match
        case Noop    => x
        case AddX(v) => x + v
    }

  def getInterestingSignalsSum(instructions: List[Instruction]) =
    List(20, 60, 100, 140, 180, 220).map(c => c * getXsUntilCycle(instructions, c).last).sum

  def renderCrt(xs: List[Int]): String =
    xs.map(x => (x - 1 to x + 1).toSet)
      .zipWithIndex
      .foldLeft(List.empty[String]) { case (acc, (range, pos)) =>
        if range(pos % 40) then "#" :: acc else " " :: acc
      }
      .reverse
      .mkString

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${getInterestingSignalsSum(input)}")
    println("Solution for Part 2:")
    renderCrt(getXsUntilCycle(input, 240)).grouped(40).foreach(println)
