package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/even-tree/problem

Depth First Search
 */
object EvenTree {
  type Graph[T] = mutable.Map[T, mutable.Set[T]]

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine().split(" ").map(_.toInt)
    val graph: Graph[Int] = mutable.Map[Int, mutable.Set[Int]]()
    for (_ <- 1 to m) {
      val Array(x, y) = io.StdIn.readLine().split(" ").map(_.toInt)
      graph.getOrElseUpdate(x, mutable.Set.empty[Int]) += y
      graph.getOrElseUpdate(y, mutable.Set.empty[Int]) += x
    }

    def dfs(v: Int, parent: Int): (Int, Int) = { // returns acc, nodes
      graph(v).filter(_ != parent).foldLeft((0, 1)) {
        case ((acc, nodes), child) =>
          val (childAcc, childNodes) = dfs(child, v)

          val isEven = childNodes > 0 && childNodes % 2 == 0

          (acc + childAcc + (if(isEven) 1 else 0), nodes + (if (isEven) 0 else childNodes))
      }
    }

    println(dfs(1, 0)._1)

  }

}
