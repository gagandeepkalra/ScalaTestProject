package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/primsmstsub/problem

Prim's Algorithm for minimum spanning tree
 */
object PrimsMSTSpecialSubtree {

  type Graph = Array[List[(Int, Int)]]

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val graph: Array[List[(Int, Int)]] = {
      val arr = Array.fill[List[(Int, Int)]](n + 1)(Nil)
      (1 to m).foreach { _ =>
        val Array(x, y, w) = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(x) = (y, w) :: arr(x)
        arr(y) = (x, w) :: arr(y)
      }

      arr.array
    }

    val root = io.StdIn.readInt()

    println {
      val visited = Array.fill(n + 1)(false)

      val queue = mutable.PriorityQueue[(Int, Int)]((root, 0))(
        Ordering.by[(Int, Int), Int](_._2).reverse
      )

      var result = 0

      while (queue.nonEmpty) {
        val (node, weight) = queue.dequeue()
        if (!visited(node)) {
          visited(node) = true
          result += weight
          graph(node).foreach {
            case (neighbour, weight) =>
              if (!visited(neighbour)) {
                queue.enqueue((neighbour, weight))
              }
          }
        }
      }

      result
    }
  }

}
