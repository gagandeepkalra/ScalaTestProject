package hackerrank

import scala.collection.immutable.Queue
import scala.collection.mutable

object ShortestReach {
  type Graph[T] = mutable.Map[T, mutable.Set[T]]

  def main(args: Array[String]): Unit = {
    val t = io.StdIn.readInt()
    for (_ <- 1 to t) {
      val Array(n, m) = io.StdIn.readLine().split(" ").map(_.toInt)
      val graph: Graph[Int] = mutable
        .Map[Int, mutable.Set[Int]]()
        .withDefaultValue(mutable.Set.empty[Int])
      for (_ <- 1 to m) {
        val Array(x, y) = io.StdIn.readLine().split(" ").map(_.toInt)
        graph.getOrElseUpdate(x, mutable.Set.empty[Int]) += y
        graph.getOrElseUpdate(y, mutable.Set.empty[Int]) += x
      }
      val s = io.StdIn.readInt()

      val distances = Array.fill(n + 1)(-1)
      val visited = Array.fill(n + 1)(false)

      @scala.annotation.tailrec
      def breadthFirstSearch(queue: Queue[(Int, Int)]): Unit = {
        if (queue.nonEmpty) {
          val ((vertex, distance), updatedQ) = queue.dequeue

          if (!visited(vertex)) distances(vertex) = distance
          visited(vertex) = true

          breadthFirstSearch(
            graph(vertex)
              .filter(!visited(_))
              .foldLeft(updatedQ)((q, i) => q.enqueue((i, distance + 6)))
          )
        }
      }

      breadthFirstSearch(Queue((s, 0)))

      println((1 to n).filter(_ != s).map(distances).mkString(" "))
    }
  }

}
