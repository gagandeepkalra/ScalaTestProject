package google.kickstart._2021.B

import algorithms.range.PersistentSegmentTree

/**
 * Charles is a truck driver in the city of Googleland. Googleland is built in form of a tree with N nodes where each node
 * represents a city and each edge represents a road between two cities. The cities are numbered 1 to N. The capital of
 * Googleland is city 1. Each day Charles picks up a load of weight W in city C and wants to deliver it to city 1 using
 * the simple path (which is unique) between the cities. Each road i has a toll which charges amount Ai if the weight of
 * the load is greater than or equal to a load-limit Li.
 *
 * Charles works for Q days, where for each day Charles will be given the starting city C and weight of the load W. For
 * each day find the greatest common divisor of all the toll charges that Charles pays for that day. If Charles did not
 * have to pay in any of the tolls the answer is 0.
 *
 * --
 *
 * we dfs from node 1, use a persistent segment tree to save tree state at each node,
 * later we query for gcd between 0 and given load
 *
 * [Segment trees, DFS]
 */
object TruckDelivery {

  import scala.annotation.tailrec

  @tailrec
  def gcd(x: Long, y: Long): Long = if (y == 0) x else gcd(y, x % y)

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)
      val (graph, maxLoad) = {
        val e: Array[List[(Int, Int, Long)]] = Array.fill[List[(Int, Int, Long)]](n + 1)(Nil)

        var maxLoad = 0

        (1 until n).foreach { _ =>
          val Array(x, y, load, cost) = io.StdIn.readLine.split(" ").map(_.toLong)

          val loadInt = load.toInt

          e(x.toInt) ::= (y.toInt, loadInt, cost)
          e(y.toInt) ::= (x.toInt, loadInt, cost)

          maxLoad = maxLoad max loadInt
        }

        (e: IndexedSeq[List[(Int, Int, Long)]], maxLoad)
      }

      // Each road i has a toll which charges amount Ai if the weight of the load is greater than or equal to a load-limit Li.

      val defaultTree: PersistentSegmentTree[Long] =
        PersistentSegmentTree(start = 0, endIncl = maxLoad.toInt, elements = _ => 0, combine = gcd)

      @tailrec
      def dfs(
        acc: Map[Int, PersistentSegmentTree[Long]], // (gcd) of all values >= load, key
        stack: List[(Int, (Int, Int, Long))]
      ): Map[Int, PersistentSegmentTree[Long]] = {
        stack match {
          case Nil => acc
          case (parent, (node, load, cost)) :: rest =>
            val parentTree = acc(parent)

            val myTree = parentTree.update(load, gcd(_, cost))

            dfs(acc + (node -> myTree), graph(node).collect { case c if c._1 != parent => node -> c } ++ rest)
        }
      }

      val segmentTreeMapForNode: Map[Int, PersistentSegmentTree[Long]] =
        dfs(Map.empty.withDefaultValue(defaultTree), graph(1).map(1 -> _))

      val result = {
        (1 to q)
          .map { _ =>
            val Array(node, load) = io.StdIn.readLine.split(" ").map(_.toInt)

            val segmentTree = segmentTreeMapForNode(node)

            segmentTree.query(0, load min maxLoad)
          }
          .mkString(" ")
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }

}
