package com.beneyal.aoc

import scala.io.Source

object day2part2:
  import Move.*
  import Outcome.*

  enum Move(val shapeScore: Int):
    case Rock extends Move(1)
    case Paper extends Move(2)
    case Scissors extends Move(3)

  object Move:
    def fromChar(c: Char): Move = c match
      case 'A' => Rock
      case 'B' => Paper
      case 'C' => Scissors
      case _   => ???

  enum Outcome:
    case Lose, Draw, Win

  object Outcome:
    def fromChar(c: Char): Outcome =
      c match
        case 'X' => Lose
        case 'Y' => Draw
        case 'Z' => Win
        case _   => ???

  final case class Game(opponent: Move, outcome: Outcome) {
    def score: Int =
      opponent match
        case Rock =>
          outcome match
            case Lose => Scissors.shapeScore
            case Draw => Rock.shapeScore + 3
            case Win  => Paper.shapeScore + 6
        case Paper =>
          outcome match
            case Lose => Rock.shapeScore
            case Draw => Paper.shapeScore + 3
            case Win  => Scissors.shapeScore + 6
        case Scissors =>
          outcome match
            case Lose => Paper.shapeScore
            case Draw => Scissors.shapeScore + 3
            case Win  => Rock.shapeScore + 6
  }

  def readInput(): Vector[Game] =
    Source
      .fromResource("day2-input.txt")
      .getLines()
      .toVector
      .map(s => Game(Move.fromChar(s(0)), Outcome.fromChar(s(2))))

  def getTotalScore(games: Vector[Game]): Int =
    games.foldLeft(0)(_ + _.score)

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Total Strategy Guide Score: ${getTotalScore(input)}")
