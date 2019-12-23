package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/journey-to-the-moon/problem
 */
object JourneyToTheMoon {
  type Graph[T] = mutable.Map[T, mutable.Set[T]]
  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine().split(" ").map(_.toInt)
    val graph: Graph[Int] = mutable
      .Map[Int, mutable.Set[Int]]()
      .withDefaultValue(mutable.Set.empty[Int])
    for (_ <- 1 to m) {
      val Array(x, y) = io.StdIn.readLine().split(" ").map(_.toInt)
      graph.getOrElseUpdate(x, mutable.Set.empty[Int]) += y
      graph.getOrElseUpdate(y, mutable.Set.empty[Int]) += x
    }

    val visited: mutable.Set[Int] = mutable.Set.empty[Int]

    def dfs(v: Int): Int = {
      if (visited(v)) 0
      else {
        visited += v
        graph(v).foldLeft(1)(_ + dfs(_))
      }
    }

    println {
      (0 until n).foldLeft(n.toLong * (n.toLong - 1) / 2)((acc, v) => {
        if (!visited(v)) {
          val c = dfs(v).toLong
          acc - c * (c - 1) / 2
        } else acc
      })
    }

  }
}
