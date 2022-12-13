package com.beneyal.aoc

import scala.io.Source
import scala.annotation.tailrec

object day13:
  import Decision.*
  import Tree.*

  enum Tree:
    case Leaf(value: Int)
    case Node(children: List[Tree])

  type Pair = (Tree, Tree)

  enum Decision:
    case Accept, Reject, Pass

    def &&(that: Decision): Decision =
      this match
        case Accept => Accept
        case Reject => Reject
        case Pass   => that

    def toBoolean: Boolean =
      this match
        case Accept => true
        case _      => false

  def readInput(): List[Pair] =
    def parseTree(s: String): Tree =
      @tailrec
      def loop(s: List[Char], stack: List[Tree], depths: List[Int]): Tree =
        if s.isEmpty then stack.head
        else
          s match
            case '[' :: rest => loop(rest, stack, 0 :: depths)
            case ']' :: rest =>
              val depth    = depths.head
              val tree     = Node(stack.take(depth).reverse)
              val newDepth = if depths.tail.isEmpty then 0 else depths.tail.head + 1
              loop(rest, tree :: stack.drop(depth), newDepth :: depths.drop(2))
            case ',' :: rest => loop(rest, stack, depths)
            case cs =>
              loop(
                cs.dropWhile(_.isDigit),
                Leaf(cs.takeWhile(_.isDigit).mkString.toInt) :: stack,
                depths.head + 1 :: depths.tail
              )

      loop(s.toList, List.empty, List.empty)

    Source.fromResource("day13-input.txt").getLines().filter(_.nonEmpty).grouped(2).toList.map { case Seq(t1, t2) =>
      (parseTree(t1), parseTree(t2))
    }

  def inRightOrder(pair: Pair): Decision =
    val (t1, t2) = pair
    (t1, t2) match
      case (Node(cs1), Node(cs2)) =>
        (cs1, cs2) match
          case (Nil, Nil)                 => Pass
          case (Nil, _)                   => Accept
          case (_, Nil)                   => Reject
          case (c1 :: rest1, c2 :: rest2) => inRightOrder((c1, c2)) && inRightOrder((Node(rest1), Node(rest2)))
      case (Leaf(v1), Leaf(v2)) => if v1 < v2 then Accept else if v1 > v2 then Reject else Pass
      case (Node(cs), Leaf(v))  => inRightOrder((Node(cs), Node(List(Leaf(v)))))
      case (Leaf(v), Node(cs))  => inRightOrder((Node(List(Leaf(v))), Node(cs)))

  def checkRightOrderIndices(pairs: List[Pair]): List[Int] =
    pairs.zipWithIndex
      .filter { case (p, i) =>
        inRightOrder(p).toBoolean
      }
      .map(_._2 + 1)

  def getDecoderKey(pairs: List[Pair]): Int =
    def divider(x: Int): Tree = Node(List(Node(List(Leaf(x)))))

    val sorted =
      pairs.appended((divider(2), divider(6))).flatMap { case (t1, t2) => List(t1, t2) }.sortWith { case (t1, t2) =>
        inRightOrder((t1, t2)).toBoolean
      }

    (sorted.indexOf(divider(2)) + 1) * (sorted.indexOf(divider(6)) + 1)

  def main(args: Array[String]): Unit =
    val input = readInput()
    println(s"Solution for Part 1: ${checkRightOrderIndices(input).sum}")
    println(s"Solution for Part 2: ${getDecoderKey(input)}")
