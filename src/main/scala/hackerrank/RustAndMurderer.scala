package hackerrank

import scala.collection.immutable.Queue

/*
https://www.hackerrank.com/challenges/rust-murderer/problem

[Breadth first search]

we keep track of remaining vertices instead of creating the complimentary graph.
Interestingly using immutable collections was faster for this problem.
 */
object RustAndMurderer {

  type Graph = Array[Set[Int]]

  def main(args: Array[String]): Unit = {
    println {
      (for (_ <- 1 to io.StdIn.readInt) yield {
        val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
        val graph: Graph = Array.fill(n + 1)(Set[Int]())

        for (_ <- 1 to m) {
          val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
          graph(x) += y
          graph(y) += x
        }

        val source = io.StdIn.readInt

        val distances = new Array[Int](n + 1)

        @scala.annotation.tailrec
        def bfs(queue: Queue[Int], notVisited: Set[Int]): Unit = {
          if (queue.nonEmpty) {
            val (vertex, afterRemovingHeadQ) = queue.dequeue
            val exclude = graph(vertex)

            val changes = notVisited.foldLeft(List.empty[Int]) {
              case (acc, i) =>
                if (!exclude(i)) {
                  distances(i) = distances(vertex) + 1
                  i :: acc
                } else acc
            }

            val (updatedQ, updatedNotVisited) = changes.foldLeft((afterRemovingHeadQ, notVisited)) {
              case ((accQ, accR), i) => (accQ.enqueue(i), accR - i)
            }

            bfs(updatedQ, updatedNotVisited)
          }
        }

        bfs(Queue(source), (1 to n).toSet - source)

        (1 to n).foldLeft(new StringBuilder)((acc, i) => if (i != source) acc.append(distances(i)).append(' ') else acc)
      }).mkString("\n")
    }
  }

}