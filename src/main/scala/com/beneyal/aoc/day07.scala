package com.beneyal.aoc

import scala.io.Source
import scala.annotation.tailrec

object day07:
  import FS.*

  sealed trait FS:
    def name: String

  object FS:
    final case class File(name: String, size: Int) extends FS
    final case class Dir(name: String, children: Vector[FS]) extends FS:
      def childrenSum: Int =
        children.collect {
          case File(name, size) => size
          case dir: Dir         => dir.childrenSum
        }.sum

  def readAndParseInput() =
    val input =
      Source.fromResource("day7-input.txt").getLines().toList.tail.filter(s => s != "$ ls" && !s.startsWith("dir"))

    @tailrec
    def loop(instructions: List[String], stack: List[Dir]): Dir =
      instructions match
        case Nil =>
          val current = stack.head
          val parent  = stack.tail.head
          parent.copy(children = parent.children :+ current)
        case head :: tail =>
          head match
            case "$ cd .." =>
              val current = stack.head
              val parent  = stack.tail.head
              loop(tail, parent.copy(children = parent.children :+ current) :: stack.drop(2))
            case s"$$ cd $dir" => loop(tail, Dir(name = dir, children = Vector.empty) :: stack)
            case s"$fileSize $fileName" =>
              val current = stack.head
              loop(tail, current.copy(children = current.children :+ File(fileName, fileSize.toInt)) :: stack.tail)

    loop(input, List(Dir(name = "/", children = Vector.empty)))

  def calculateDirectorySizesSum(root: Dir): Int =
    val childrenSum   = root.childrenSum
    val childrenSizes = root.children.collect { case d: Dir => d }.map(calculateDirectorySizesSum).sum
    if childrenSum <= 100000 then childrenSum + childrenSizes
    else childrenSizes

  def sizeOfSmallestDirectoryToDelete(root: Dir): Int =
    val needToFree = 30000000 - (70000000 - root.childrenSum)

    def deletionCandidates(dir: Dir): Vector[Int] =
      val childrenSum        = dir.childrenSum
      val childrenCandidates = dir.children.collect { case d: Dir => d }.flatMap(deletionCandidates)
      if childrenSum >= needToFree then Vector(childrenSum) ++ childrenCandidates
      else childrenCandidates

    deletionCandidates(root).min

  def main(args: Array[String]): Unit =
    val tree = readAndParseInput()
    println(s"Solution for Part 1: ${calculateDirectorySizesSum(tree)}")
    println(s"Solution for Part 2: ${sizeOfSmallestDirectoryToDelete(tree)}")
