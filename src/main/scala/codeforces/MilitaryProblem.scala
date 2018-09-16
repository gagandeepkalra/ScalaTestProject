package codeforces

import scala.collection.Searching._
import scala.collection.mutable

/*

link- http://codeforces.com/contest/1006/problem/E

Pre-order traversal sequence, find the kth element starting from node u

 */
object MilitaryProblem {

  def main(args: Array[String]): Unit = {
    val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)
    val parentArray: Array[Int] = io.StdIn.readLine.split(" ").map(_.toInt)

    val children: Array[mutable.Queue[Int]] = Array.fill(n + 1)(mutable.Queue[Int]())

    computeChildrenMapping(0)

    def computeChildrenMapping(idx: Int): Unit = {
      if (idx < parentArray.length) {
        children(parentArray(idx)).enqueue(idx + 2)
        computeChildrenMapping(idx + 1)
      }
    }

    val sortedSeq, startingPositionInSortedSeq, endingPositionInSortedSeq = new Array[Int](n + 1)
    var i = 0
    dfs(1)

    def dfs(node: Int): Unit = {
      sortedSeq(i) = node
      startingPositionInSortedSeq(node) = i
      i += 1
      children(node).foreach(dfs)
      endingPositionInSortedSeq(node) = i - 1
    }

    solveQueries(0)

    def solveQueries(idx: Int): Unit = {
      if (idx < q) {
        val Array(u, k) = io.StdIn.readLine.split(" ").map(_.toInt)

        val startingPos = startingPositionInSortedSeq(u)
        val endingPos = endingPositionInSortedSeq(u)

        if (endingPos - startingPos + 1 < k) println(-1)
        else println(sortedSeq(startingPos + k - 1))

        solveQueries(idx + 1)
      }
    }

  }


  // Another Solution although inefficient- compute size of each subtree then for each query find appropriate subtree
  // and recurse with a new u and k

  def usingReverseLevelOrderTraversal(args: Array[String]): Unit = {
    val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)

    val parentArray: Array[Int] = Array(0, 0) ++ io.StdIn.readLine.split(" ").map(_.toInt)
    var childrenMap: Map[Int, Array[Int]] = parentArray.zipWithIndex.groupBy(_._1).map { case (p, elems) => (p, elems.unzip._2) }

    val subtreeSizeCount: Array[Int] = { // reverse level order traversal to compute sizes of each subtree
      val result = Array.fill(n + 1)(1)

      def compute(set: Set[Int]): Unit = {
        set.foreach(node => result(parentArray(node)) += result(node))
        if (!(set.size == 1 && set.contains(0))) compute(set map parentArray)
      }

      compute((1 to n).filter(node => !childrenMap.contains(node)).toSet) // starting with the leaves
      result
    }

    val childrenMapWithPrefixSum = childrenMap.map { case (node, children) => (node, children.scanLeft(1) { case (res, elem) => res + subtreeSizeCount(elem) }) }

    var i = 0
    while (i < q) {
      val Array(u, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      println(computeQueryResult(u, k))
      i += 1
    }

    def computeQueryResult(u: Int, k: Int): Int = {
      if (k > subtreeSizeCount(u)) -1
      else if (k == 1) u
      else {
        val childPrefixSum: Array[Int] = childrenMapWithPrefixSum(u)
        val i = childPrefixSum.search(k).insertionPoint // optimized with binary search
        val newU = childrenMap(u)(i - 1)
        val newK = k - childPrefixSum(i - 1)
        computeQueryResult(newU, newK)
      }
    }
  }
}
