package com.beneyal.aoc

import scala.io.Source

object day2part1:
  import Move.*

  enum Move(val shapeScore: Int):
    case Rock extends Move(1)
    case Paper extends Move(2)
    case Scissors extends Move(3)

  object Move:
    def fromChar(c: Char): Move = c match
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors
      case _         => ???

  final case class Game(opponent: Move, me: Move) {
    def score: Int =
      me.shapeScore + ((me, opponent) match
        case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 0
        case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => 6
        case _                                                    => 3
      )
  }

  def readInput(): Vector[Game] =
    Source
      .fromResource("day2-input.txt")
      .getLines()
      .toVector
      .map(s => Game(Move.fromChar(s(0)), Move.fromChar(s(2))))

  def getTotalScore(games: Vector[Game]): Int =
    games.foldLeft(0)(_ + _.score)

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Total Strategy Guide Score: ${getTotalScore(input)}")
