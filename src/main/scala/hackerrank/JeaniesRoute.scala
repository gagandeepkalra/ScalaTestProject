package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/jeanies-route/problem

[Recursion]

Travelling salesman problem for a undirected acyclic graph

twice the weight - diameter cost
 */
object JeaniesRoute {

  type Graph = Array[List[(Int, Int)]]

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val isChosenOne = io.StdIn.readLine.split(" ").map(_.toInt).toSet

    val graph: Array[List[(Int, Int)]] = {
      val arr = Array.fill[List[(Int, Int)]](n + 1)(Nil)
      (1 until n).foreach { _ =>
        val Array(x, y, w) = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(x) = (y, w) :: arr(x)
        arr(y) = (x, w) :: arr(y)
      }

      arr.array
    }

    val anyChosenOne = isChosenOne.head

    val reducedGraph: Graph = {
      val arr: Array[List[(Int, Int)]] = graph.clone()

      def dfs(p: Int, n: Int, pnW: Int): Boolean = {
        val (accL, accB) = graph(n).foldLeft((List.empty[(Int, Int)], isChosenOne(n))) {
          case ((accL, accB), rec@(c, w)) =>
            if (c != p && dfs(n, c, w))
              (rec :: accL, true)
            else
              (accL, accB)
        }


        arr(n) = if (accB) (p, pnW) :: accL else accL
        accB
      }

      dfs(0, anyChosenOne, 0)
      arr
    }

    def findDiameter(p: Int, n: Int): (Int, Int) = { // diameter and height
      val ls = reducedGraph(n).collect { case (c, w) if c != p => (w, findDiameter(n, c)) }

      if (ls.isEmpty) (0, 0)
      else {
        val (_, (mD, _)) = ls.maxBy(_._2._1)

        val heightSortedList = ls.sortBy(x => x._1 + x._2._2)(Ordering[Int].reverse)

        val (mHEdgeCost, (_, mH)) = heightSortedList.head
        val (smHEdgeCost, (_, smH)) = heightSortedList.tail.headOption.getOrElse((0, (0, 0)))

        val newMaxDiameter = (mH + mHEdgeCost + smHEdgeCost + smH) max mD

        (newMaxDiameter, mHEdgeCost + mH)
      }
    }

    val diameter = findDiameter(0, anyChosenOne)._1

    val totalWeight = reducedGraph.foldLeft(0)(_ + _.foldLeft(0)(_ + _._2))

    println(totalWeight - diameter)
  }


  /*
  Heavier solution:
    1. for each edge p -> n calculate round trip weight from n and persist
    2. from each chosen one n find the path to the other end by following only the maxEdgeCost from n and adding the rest
    3. min #2 be the solution
   */
  def solve(n: Int, graph: Graph, isChosenOne: Set[Int]): Unit = {

    val edgeCost: mutable.Map[(Int, Int), Int] = {
      val m = collection.mutable.Map.empty[(Int, Int), Int] // edge -> cost

      def preComputeR(parent: Int, n: Int, cost: Int): Int =
        m.getOrElseUpdate(parent -> n, {
          val res = graph(n).foldLeft(0) {
            case (acc, (child, weight)) =>
              acc + (if (child != parent) preComputeR(n, child, weight) else 0)
          }

          res + (if (res != 0 || isChosenOne(n)) 2 * cost else 0)
        })

      (1 to n).foreach { i =>
        graph(i).foreach { case (x, y) =>
          preComputeR(i, x, y)
        }
      }

      m
    }

    @scala.annotation.tailrec
    def dfs(parent: Int, n: Int, acc: Int = 0): Int = {
      val edgeCosts = graph(n).collect { case (c, w) if c != parent => (c, w, edgeCost(n -> c)) }
      if (edgeCosts.isEmpty) acc
      else {
        val (maxC, maxCWeight, maxCost) = edgeCosts.maxBy(_._3)
        if (maxCost == 0) acc
        else {
          val otherCost = edgeCosts.foldLeft(0)(_ + _._3) - maxCost
          dfs(n, maxC, acc + otherCost + maxCWeight)
        }
      }
    }

    println(isChosenOne.map(dfs(0, _)).min)
  }

}
