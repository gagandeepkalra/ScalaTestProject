package codeforces.contests._1433

/*
https://codeforces.com/contest/1433/problem/G

[Dijkstra]

Think like an edge.

We try zeroing out each edge, afterwards we compare if the shortest distance did get affected.
 */
object ReducingDeliveryCost {

  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val (graph, allEdges) = {
      val g = Array.fill[List[(Int, Int)]](n + 1)(Nil)
      val e = new Array[(Int, Int)](m)

      (0 until m).foreach { i =>
        val Array(x, y, c) = io.StdIn.readLine.split(" ").map(_.toInt)
        g(x) ::= (y, c)
        g(y) ::= (x, c)

        e(i) = (x, y)
      }

      (g, e)
    }

    val distance = Array.fill[Int](n + 1, n + 1)(Int.MaxValue)

    val queue = mutable.PriorityQueue.empty[(Int, Int)](
      Ordering.by[(Int, Int), Int](_._2).reverse
    )

    def dijkstra(start: Int): Unit = {
      distance(start)(start) = 0
      queue.enqueue((start, 0))

      while (queue.nonEmpty) {
        val (current, givenCost) = queue.dequeue()
        if (givenCost == distance(start)(current)) {
          graph(current).foreach {
            case (neighbour, weight) =>
              val neighbourCost = givenCost + weight
              if (neighbourCost < distance(start)(neighbour)) {
                distance(start)(neighbour) = neighbourCost
                queue.enqueue((neighbour, neighbourCost))
              }
          }
        }
      }
    }

    def calculateCost(start: Int, end: Int, edge_a: Int, edge_b: Int): Int =
      distance(start)(end) min
        (distance(start)(edge_a) + distance(end)(edge_b)) min
        (distance(start)(edge_b) + distance(end)(edge_a))

    val (pairs, allStarts) = {
      val all = mutable.Set.empty[Int]
      ((1 to k).map { _ =>
        val Array(s, e) = io.StdIn.readLine.split(" ").map(_.toInt)
        all += s += e
        (s, e)
      }, all)
    }

    allStarts.foreach(dijkstra)

    // for each edge, we try relax
    val result = allEdges.foldLeft(Int.MaxValue) {
      case (acc, (u, v)) =>
        acc min pairs.foldLeft(0) { (sum, p) =>
          sum + calculateCost(p._1, p._2, u, v)
        }
    }

    println(result)
  }
}
