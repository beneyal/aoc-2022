package com.beneyal.aoc

import scala.io.Source

object day5:
  type CrateStack = Vector[Vector[Option[Char]]]
  final case class Move(amount: Int, from: Int, to: Int)

  def readAndParseInput(): (CrateStack, Vector[Move]) =
    val movePattern = """move (\d+) from (\d+) to (\d+)""".r

    def parseCrateLine(s: String): Vector[Option[Char]] =
      s.grouped(4).toVector.map { g =>
        if g.trim.nonEmpty then Some(g(1)) else None
      }

    def parseMove(s: String): Move = s match
      case movePattern(amount, from, to) =>
        Move(amount.toInt, from.toInt - 1, to.toInt - 1)
      case _ => ???

    val inputs = Source
      .fromResource("day5-input.txt")
      .getLines()
      .mkString("\n")
      .split("\n\n")
      .toVector
    val initialStacks = inputs(0).split('\n').init.map(parseCrateLine)
    val moves = inputs(1).split('\n').map(parseMove)

    (initialStacks.toVector.transpose, moves.toVector)

  def performMoves(
      initialStacks: CrateStack,
      moves: Vector[Move],
      transform: Vector[Option[Char]] => Vector[Option[Char]] = identity
  ) =
    moves.foldLeft(initialStacks) { case (stacks, Move(amount, from, to)) =>
      val f = stacks(from).dropWhile(_.isEmpty)
      stacks
        .updated(
          to,
          transform(f.take(amount)) ++ stacks(to).dropWhile(_.isEmpty)
        )
        .updated(from, f.drop(amount))
    }

  def main(args: Array[String]): Unit =
    val (stacks, moves) = readAndParseInput()
    val updatedStacksPart1 = performMoves(stacks, moves, _.reverse)
    val updatedStacksPart2 = performMoves(stacks, moves)
    println(
      s"Solution for Part 1: ${updatedStacksPart1.map(_.dropWhile(_.isEmpty).head).map(_.get).mkString}"
    )
    println(
      s"Solution for Part 2: ${updatedStacksPart2.map(_.dropWhile(_.isEmpty).head).map(_.get).mkString}"
    )
