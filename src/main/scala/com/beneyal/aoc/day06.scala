package com.beneyal.aoc

import scala.io.Source

object day06:
  import MarkerType.*

  enum MarkerType:
    case StarOfPacket, StartOfMessage

  def readInput(): Vector[Char] =
    Source.fromResource("day6-input.txt").getLines().toVector.head.toVector

  def getMarker(buffer: Vector[Char], markerType: MarkerType) =
    val n = markerType match
      case StarOfPacket   => 4
      case StartOfMessage => 14

    buffer.sliding(n).map(_.toSet).zipWithIndex.find(_._1.size == n).get._2 + n

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${getMarker(input, StarOfPacket)}")
    println(s"Solution for Part 2: ${getMarker(input, StartOfMessage)}")
