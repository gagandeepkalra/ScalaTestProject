package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/beautiful-path/problem

Dijkstra, given cost is added using the OR operator

Consider a point B along the optimal path from A to C. Joining the optimal paths from A to B and B to C may not give you
the optimal solution for A to C because of of how OR works.

A locally non-optimal choice may be best when combined with later forced choices, ie. 2 OR 2 is less than 1 OR 2.
 */
object MinimumPenaltyPath {

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val graph: Array[List[(Int, Int)]] = {
      val arr = Array.fill[List[(Int, Int)]](n + 1)(Nil)
      (1 to m).foreach { _ =>
        val Array(x, y, w) = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(x) = (y, w) :: arr(x)
        arr(y) = (x, w) :: arr(y)
      }
      arr
    }

    val Array(start, destination) = io.StdIn.readLine.split(" ").map(_.toInt)

    val maxK = 1 << 10

    val visited = Array.fill(n + 1, maxK)(false)

    val queue = mutable.PriorityQueue[(Int, Int)]((start, 0))(
      Ordering.by[(Int, Int), Int](_._2).reverse
    )

    @scala.annotation.tailrec
    def recur(): Unit = if (queue.nonEmpty) {
      val (nn, givenCost) = queue.dequeue()
      graph(nn).foreach {
        case (neighbour, weight) =>
          val neighbourCost = givenCost | weight
          if (!visited(neighbour)(neighbourCost)) {
            visited(neighbour)(neighbourCost) = true
            queue.enqueue((neighbour, neighbourCost))
          }
      }
      recur()
    }

    recur()

    println {
      (1 until maxK).find(visited(destination)(_)).getOrElse(-1)
    }
  }

}
