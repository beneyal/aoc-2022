package com.beneyal.aoc

import scala.io.Source

object day11:
  final case class Monkey(
      items: Vector[Long],
      op: Long => Long,
      divisor: Int,
      ifTrueMonkey: Int,
      ifFalseMonkey: Int
  )

  val monkeyPattern = """Monkey \d:
    |  Starting items: (.*)
    |  Operation: (.*)
    |  Test: divisible by (\d+)
    |    If true: throw to monkey (\d)
    |    If false: throw to monkey (\d)""".stripMargin.r

  def readInput() =
    def parseOp(s: String): Long => Long =
      val Array(_, rhs) = s.split(" = ")
      rhs match
        case s"old + $rand" => if rand == "old" then (old => 2 * old) else (old => old + rand.toLong)
        case s"old * $rand" => if rand == "old" then (old => old * old) else (old => old * rand.toLong)
        case _              => ???

    Source
      .fromResource("day11-input.txt")
      .getLines()
      .mkString("\n")
      .split("\n\n")
      .flatMap { s =>
        monkeyPattern
          .findAllMatchIn(s)
          .map(_.subgroups)
          .collect { case List(items, op, div, ifTrue, ifFalse) =>
            val parsedItems = items.split(", ").map(_.toLong).toVector
            val parsedOp    = parseOp(op)
            Monkey(parsedItems, parsedOp, div.toInt, ifTrue.toInt, ifFalse.toInt)
          }
      }
      .toVector

  def runRounds(monkeys: Vector[Monkey], times: Int, f: Long => Long) =
    def loop(monkeys: Vector[Monkey], currentMonkey: Int, roundNum: Int, touched: Map[Int, Int]): Map[Int, Int] =
      val monkey = monkeys(currentMonkey)
      if roundNum == times then touched
      else if monkey.items.isEmpty
      then
        val updatedRoundNum = if currentMonkey == monkeys.length - 1 then roundNum + 1 else roundNum
        loop(monkeys, (currentMonkey + 1) % monkeys.length, updatedRoundNum, touched)
      else
        // val worryLevel    = monkey.op(monkey.items.head) / 3
        val worryLevel    = f(monkey.op(monkey.items.head))
        val target        = if worryLevel % monkey.divisor == 0 then monkey.ifTrueMonkey else monkey.ifFalseMonkey
        val targetMonkey  = monkeys(target)
        val updatedTarget = targetMonkey.copy(items = targetMonkey.items.appended(worryLevel))
        val updatedMonkeys = monkeys
          .updated(target, updatedTarget)
          .updated(currentMonkey, monkey.copy(items = monkey.items.tail))
        loop(updatedMonkeys, currentMonkey, roundNum, touched + (currentMonkey -> (touched(currentMonkey) + 1)))

    loop(monkeys, 0, 0, monkeys.indices.map(_ -> 0).toMap)

  def main(args: Array[String]): Unit =
    val input         = readInput()
    val part1Solution = runRounds(input, 20, _ / 3).toList.map(_._2).sorted(using Ordering[Int].reverse).take(2).product
    println(s"Solution for Part 1: ${part1Solution}")
    val divisor = input.map(_.divisor).product
    val part2Solution = runRounds(input, 10000, _ % divisor).toList
      .map(_._2)
      .sorted(using Ordering[Int].reverse)
      .take(2)
      .map(_.toLong)
      .product
    println(s"Solution for Part 2: ${part2Solution}")
