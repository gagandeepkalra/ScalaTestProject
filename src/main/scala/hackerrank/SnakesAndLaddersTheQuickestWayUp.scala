package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/the-quickest-way-up/problem

Dijkstra
 */
object SnakesAndLaddersTheQuickestWayUp {

  def inputPairs(n: Int): Map[Int, Int] =
    (1 to n).map { _ =>
      val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
      (x, y)
    }.toMap

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()
      val ladders = inputPairs(n)
      val m = io.StdIn.readInt()
      val snakes = inputPairs(m)

      val distance = Array.fill(101)(Int.MaxValue)

      val queue = mutable.PriorityQueue[(Int, Int)]((1, 0))(
        Ordering.by[(Int, Int), Int](_._2).reverse
      )

      distance(1) = 0

      while (queue.nonEmpty) {
        val (node, nodeDistance) = queue.dequeue()
        if (nodeDistance == distance(node)) {
          ladders
            .get(node)
            .orElse(snakes.get(node))
            .fold((1 to 6).foreach { offset =>
              val neighbour = node + offset
              val neighbourDistance = nodeDistance + 1
              if (neighbour <= 100 && neighbourDistance < distance(neighbour)) {
                distance(neighbour) = neighbourDistance
                queue.enqueue((neighbour, neighbourDistance))
              }
            }) { neighbour =>
              if (nodeDistance < distance(neighbour)) {
                distance(neighbour) = nodeDistance
                queue.enqueue((neighbour, nodeDistance))
              }
            }
        }

      }
      println {
        if (distance(100) != Int.MaxValue) distance(100) else -1
      }
    }
  }

}
