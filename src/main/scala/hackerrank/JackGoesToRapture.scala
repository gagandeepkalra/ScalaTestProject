package hackerrank

import scala.collection.mutable

/*
https://www.hackerrank.com/challenges/jack-goes-to-rapture/problem

Dijkstra, given cost is added using the Max operator
 */
object JackGoesToRapture {

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

    val start = 1
    val destination = n

    val visited = Array.fill(n + 1)(false)
    val cost = Array.fill(n + 1)(Int.MaxValue)

    val queue = mutable.PriorityQueue[(Int, Int)]((start, 0))(
      Ordering.by[(Int, Int), Int](_._2).reverse
    )

    @scala.annotation.tailrec
    def recur: String =
      if (queue.nonEmpty) {
        val (nn, givenCost) = queue.dequeue()
        visited(nn) = true
        if (nn == destination)
          givenCost.toString
        else {
          graph(nn).foreach {
            case (neighbour, weight) =>
              val neighbourCost = givenCost max weight
              if (!visited(neighbour) && neighbourCost < cost(neighbour)) {
                cost(neighbour) = neighbourCost
                queue.enqueue((neighbour, neighbourCost))
              }
          }
          recur
        }
      } else "NO PATH EXISTS"

    println(recur)

  }

}
