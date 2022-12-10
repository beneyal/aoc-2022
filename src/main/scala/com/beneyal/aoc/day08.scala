package com.beneyal.aoc

import scala.io.Source

object day08:
  def readInput(): Vector[Vector[Int]] =
    Source.fromResource("day8-input.txt").getLines().map(_.toVector.map(_.toString.toInt)).toVector

  def isVisible(grid: Vector[Vector[Int]], i: Int, j: Int): Boolean =
    val (rowBefore, rowFrom) = grid(i).splitAt(j)
    val (colBefore, colFrom) = grid.map(_(j)).splitAt(i)
    val tree                 = grid(i)(j)
    rowBefore.max < tree || rowFrom.tail.max < tree || colBefore.max < tree || colFrom.tail.max < tree

  def getVisibleTrees(grid: Vector[Vector[Int]]): Int =
    val edges = 2 * grid.length + 2 * grid(0).length - 4
    edges + grid.zipWithIndex.slice(1, grid.length - 1).foldLeft(0) { case (acc, (row, i)) =>
      acc + row.zipWithIndex.slice(1, row.length - 1).foldLeft(0) { case (acc0, (_, j)) =>
        if isVisible(grid, i, j) then acc0 + 1 else acc0
      }
    }

  def getScenicScore(grid: Vector[Vector[Int]], i: Int, j: Int): Int =
    val (rowBefore, rowFrom) = grid(i).splitAt(j)
    val (colBefore, colFrom) = grid.map(_(j)).splitAt(i)
    val tree                 = grid(i)(j)
    val top                  = colBefore.reverse.takeWhile(_ < tree).length
    val right                = rowFrom.tail.takeWhile(_ < tree).length
    val bottom               = colFrom.tail.takeWhile(_ < tree).length
    val left                 = rowBefore.reverse.takeWhile(_ < tree).length

    math.min(top + 1, colBefore.length) *
      math.min(right + 1, rowFrom.length - 1) *
      math.min(bottom + 1, colFrom.length - 1) *
      math.min(left + 1, rowBefore.length)

  def getMaxScenicScore(grid: Vector[Vector[Int]]): Int =
    grid.zipWithIndex.slice(1, grid.length - 1).foldLeft(0) { case (acc, (row, i)) =>
      math.max(
        acc,
        row.zipWithIndex.slice(1, row.length - 1).foldLeft(0) { case (acc0, (_, j)) =>
          math.max(acc0, getScenicScore(grid, i, j))
        }
      )
    }

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${getVisibleTrees(input)}")
    println(s"Solution for Part 2: ${getMaxScenicScore(input)}")
