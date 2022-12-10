package com.beneyal.aoc

import scala.io.Source

object day02:
  import Move.*

  enum Move(val shapeScore: Int):
    case Rock     extends Move(1)
    case Paper    extends Move(2)
    case Scissors extends Move(3)

  def readInput(): Vector[String] =
    Source.fromResource("day2-input.txt").getLines().toVector

  trait Game:
    def score: Int

  def getTotalScore(games: Vector[Game]): Int =
    games.foldLeft(0)(_ + _.score)

  object part1:
    def charToMove(c: Char): Move =
      c match
        case 'A' | 'X' => Rock
        case 'B' | 'Y' => Paper
        case 'C' | 'Z' => Scissors
        case _         => ???

    final case class PartOneGame(opponent: Move, me: Move) extends Game:
      def score: Int =
        me.shapeScore + ((me, opponent) match
          case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 0
          case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => 6
          case _                                                    => 3
        )

  object part2:
    import Outcome.*

    enum Outcome:
      case Lose, Draw, Win

    object Outcome:
      def fromChar(c: Char): Outcome =
        c match
          case 'X' => Lose
          case 'Y' => Draw
          case 'Z' => Win
          case _   => ???

    final case class PartTwoGame(opponent: Move, outcome: Outcome) extends Game:
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

    def charToMove(c: Char): Move =
      c match
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors
        case _   => ???

  def main(args: Array[String]): Unit =
    val input = readInput()
    val part1Input = input.map { s =>
      part1.PartOneGame(part1.charToMove(s(0)), part1.charToMove(s(2)))
    }
    val part2Input = input.map { s =>
      part2.PartTwoGame(part2.charToMove(s(0)), part2.Outcome.fromChar(s(2)))
    }
    println(s"Solution for Part 1: ${part1Input.foldLeft(0)(_ + _.score)}")
    println(s"Solution for Part 2: ${part2Input.foldLeft(0)(_ + _.score)}")
