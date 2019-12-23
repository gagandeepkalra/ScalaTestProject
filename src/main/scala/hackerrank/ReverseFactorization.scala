package hackerrank

import scala.collection.immutable.Queue

/*
https://www.hackerrank.com/challenges/reverse-factorization/problem
 */
object ReverseFactorization {

  def main(args: Array[String]): Unit = {
    // Input
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
    val factors = io.StdIn.readLine.split(" ").map(_.toInt).sorted.toList

    breadthFirstSearch(Queue(1), Map(1 -> 1))

    // Functional BFS
    @scala.annotation.tailrec
    def breadthFirstSearch(queue: Queue[Int], parentMap: Map[Int, Int]): Unit = {
      if (queue.isEmpty) println(-1)
      else if (queue.front == n) print_result(n)(parentMap)
      else {
        val (element, q1) = queue.dequeue
        val parentMultiplier = parentMap(element)

        val (q, m) = factors
          .filter(i => i >= parentMultiplier && element * i <= n && !parentMap.contains(element * i))
          .foldLeft[(Queue[Int], Map[Int, Int])]((q1, parentMap)) {
          case ((queue: Queue[Int], parentMap: Map[Int, Int]), i: Int) => (queue.enqueue(element * i), parentMap + (element * i -> i))
        }

        breadthFirstSearch(q, m)
      }
    }

    def print_result(x: Int)(implicit parentMap: Map[Int, Int]): Unit = {
      if (x != 1) print_result(x / parentMap(x))
      print(x + " ")
    }
  }

}
