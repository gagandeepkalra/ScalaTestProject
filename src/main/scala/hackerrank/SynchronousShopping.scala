package hackerrank

/*
https://www.hackerrank.com/challenges/synchronous-shopping/problem

Dijkstra
 */
object SynchronousShopping {
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val fishTypes: Array[Int] = {
      val arr = new Array[Int](n + 1)
      for (i <- 1 to n)
        arr(i) = io.StdIn.readLine
          .split(" ")
          .map(_.toInt)
          .tail
          .foldLeft(0)((acc, v) => acc | (1 << (v - 1)))
      arr
    }

    val graph: Array[List[(Int, Int)]] = {
      val arr = Array.fill[List[(Int, Int)]](n + 1)(Nil)
      (1 to m).foreach { _ =>
        val Array(x, y, w) = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(x) = (y, w) :: arr(x)
        arr(y) = (x, w) :: arr(y)
      }
      arr
    }

    val maxK = (1 << k) - 1

    val cost = Array.fill(n + 1, maxK + 1)(Int.MaxValue)

    val visited = Array.fill(n + 1, maxK + 1)(false)

    val queue = mutable.PriorityQueue[(Int, Int, Int)]((1, fishTypes(1), 0))(
      Ordering.by[(Int, Int, Int), Int](_._3).reverse
    )

    while (queue.nonEmpty) {
      val (nn, kk, givenCost) = queue.dequeue()
      if (!visited(nn)(kk)) {
        cost(nn)(kk) = givenCost
        visited(nn)(kk) = true
        graph(nn).foreach {
          case (neighbour, weight) =>
            val neighbourK = kk | fishTypes(neighbour)
            val neighbourCost = givenCost + weight
            if (!visited(neighbour)(neighbourK) && neighbourCost < cost(neighbour)(neighbourK)) {
              cost(neighbour)(neighbourK) = neighbourCost
              queue.enqueue((neighbour, neighbourK, neighbourCost))
            }
        }
      }
    }

    val result = {
      var ans = Int.MaxValue
      for (i <- 0 to maxK; j <- i to maxK; if (i | j) == maxK)
        ans = ans min (cost(n)(i) max cost(n)(j))
      ans
    }

    println(result)
  }

}
