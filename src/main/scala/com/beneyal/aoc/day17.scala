package com.beneyal.aoc

import scala.io.Source

object day17:
  import Action.*
  import Direction.*
  import Shape.*

  final case class Point(x: Int, y: Int)

  enum Action:
    case Fall
    case Push(dir: Direction)

  enum Direction(val dx: Int):
    case Left  extends Direction(-1)
    case Right extends Direction(1)

  enum Shape:
    case Minus, Plus, Ell, Beam, Block

    def coords(minY: Int): List[Point] =
      val leftX = 2
      this match
        case Minus =>
          (leftX until leftX + 4).map(Point(_, minY)).toList
        case Plus =>
          List(
            Point(leftX + 1, minY + 2),
            Point(leftX, minY + 1),
            Point(leftX + 1, minY + 1),
            Point(leftX + 2, minY + 1),
            Point(leftX + 1, minY)
          )
        case Ell =>
          List(
            Point(leftX + 2, minY + 2),
            Point(leftX + 2, minY + 1),
            Point(leftX, minY),
            Point(leftX + 1, minY),
            Point(leftX + 2, minY)
          )
        case Beam =>
          (minY to minY + 3).map(Point(leftX, _)).toList
        case Block =>
          List(leftX, leftX + 1).flatMap(x => List(minY, minY + 1).map(y => Point(x, y)))

  type Step = (Shape, List[Int])

  def readInput(): Vector[Direction] =
    Source.fromResource("day17-input.txt").getLines().toList.head.toVector.map {
      case '<' => Left
      case '>' => Right
    }

  val emptyTower = Vector.fill(3)(Vector.fill(7)(false))

  extension [A](self: LazyList[A])
    def interleave(other: LazyList[A]): LazyList[A] =
      self.head #:: (other interleave self.tail)

  extension (tower: Vector[Vector[Boolean]]) def height: Long = tower.indexWhere(_.forall(!_))

  def simulate(wind: Vector[Direction], totalRocks: Long): Long =
    lazy val shapes: LazyList[Shape] = Shape.values.to(LazyList) #::: shapes
    val continuousWind               = LazyList.from(0).map(i => wind(i % wind.length))
    val initialActions               = continuousWind.map(Push(_)) interleave LazyList.continually(Fall)
    val windowSize                   = 25

    def loop(
        rocks: Long,
        shapes: LazyList[Shape],
        actions: LazyList[Action],
        currentCoords: List[Point],
        tower: Vector[Vector[Boolean]],
        prevStep: Option[Step],
        window: Vector[(Step, Step)],
        path: Vector[((Step, Step), Long, Long)]
    ): Long =
      if rocks == totalRocks
      then tower.height
      else
        val shape  = shapes.head
        val action = actions.head

        action match
          case Fall =>
            if currentCoords.filter(_.y < tower.size).forall(p => p.y > 0 && !tower(p.y - 1)(p.x))
            then
              val newCoords = currentCoords.map(p => p.copy(y = p.y - 1))
              loop(rocks, shapes, actions.tail, newCoords, tower, prevStep, window, path)
            else
              val maxY = currentCoords.map(_.y).max + 1
              val newRows =
                if maxY + 3 < tower.size
                then Vector.empty
                else Vector.fill(maxY + 3 - tower.size)(Vector.fill(7)(false))
              val newTower = currentCoords.foldLeft(tower ++ newRows) { case (t, p) =>
                t.updated(p.y, t(p.y).updated(p.x, true))
              }
              loop(rocks + 1, shapes.tail, actions.tail, List.empty, newTower, prevStep, window, path)
          case Push(dir) =>
            val coords = if currentCoords.isEmpty then shape.coords(tower.size) else currentCoords
            val canMove = (dir match
              case Left  => coords.map(_.x).min > 0
              case Right => coords.map(_.x).max < 6
            ) && coords.filter(_.y < tower.size).forall(p => !tower(p.y)(p.x + dir.dx))
            val newCoords = if canMove then coords.map(p => p.copy(x = p.x + dir.dx)) else coords

            val step = (shape, newCoords.map(_.x))

            val maybeNewPath = prevStep
              .flatMap { pv =>
                val steps = path.map(_._1)
                if steps.size > windowSize && window.size == windowSize &&
                  steps.sliding(windowSize).count(_ == window) > 2
                then None
                else Some(path :+ ((pv, step), rocks, tower.height))
              }
            val newWindow = prevStep
              .map { pv =>
                if window.size < windowSize then window :+ (pv, step)
                else window.tail :+ (pv, step)
              }
              .getOrElse(window)

            if prevStep.isDefined && maybeNewPath.isEmpty then
              val steps           = path.map(_._1)
              val windows         = steps.sliding(windowSize).toVector
              val cycleStart      = windows.indexWhere(_ == window)
              val cycleMiddle1    = windows.indexWhere(_ == window, cycleStart + 1)
              val cycleMiddle2    = windows.indexWhere(_ == window, cycleMiddle1 + 1)
              val (_, r1, h1)     = path(cycleStart)
              val (_, r2, h2)     = path(cycleMiddle1)
              val (_, r3, h3)     = path(cycleMiddle2)
              val deltaRocks      = r3 - r2
              val deltaHeight     = h3 - h2
              val remainingCycles = totalRocks / deltaRocks
              val remainingRocks  = (totalRocks - (r1 + remainingCycles * deltaRocks))
              val remainingHeight = path
                .filter { case (_, r, _) => r == r1 + remainingRocks }
                .map { case (_, _, h) => h - h1 }
                .max
              h1 + deltaHeight * remainingCycles + remainingHeight
            else if prevStep.isEmpty then
              loop(rocks, shapes, actions.tail, newCoords, tower, Some(step), newWindow, path)
            else loop(rocks, shapes, actions.tail, newCoords, tower, Some(step), newWindow, maybeNewPath.get)

    loop(0, shapes, initialActions, List.empty, emptyTower, None, Vector.empty, Vector.empty)

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${simulate(input, 2022)}")
    println(s"Solution for Part 2: ${simulate(input, 1_000_000_000_000L)}")
