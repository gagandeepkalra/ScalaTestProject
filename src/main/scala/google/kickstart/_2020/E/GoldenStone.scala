package google.kickstart._2020.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff47/00000000003bef29

Dijkstra, multiple start nodes

We aim to fill the matrix dp[i][j] = cost of moving a stone type j to ith node, afterwards the result is max dp[i][1]

We start out with the minimum distance zero nodes (input) afterwards we relax and update children in each step, we don't
just relax the child nodes, we use the recipes too.

This question is interesting when ou think dijkstra is single source shortest path, but the the given problem reduces to
Dijkstra even with multiple start nodes, if we think again about the graph, we have multiple graphs each for single stone type but
they all intersect using the recipes.
 */
object GoldenStone {

  import scala.collection.mutable
  import scala.collection.mutable.ArrayBuffer

  val INF: Long = 1e12.toLong

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, m, s, r) = io.StdIn.readLine.split(" ").map(_.toInt)

      val graph: IndexedSeq[Seq[Int]] = {
        val g = Array.fill(n + 1)(ArrayBuffer.empty[Int])
        (1 to m).foreach { _ =>
          val Array(u, v) = io.StdIn.readLine.split(" ").map(_.toInt)
          g(u) += v
          g(v) += u
        }

        g
      }

      val stones: IndexedSeq[Seq[Int]] = {
        val st = new Array[Seq[Int]](n + 1)
        (1 to n).foreach { i =>
          val arr = io.StdIn.readLine.split(" ").map(_.toInt)
          st(i) = arr.view(1, arr(0) + 1)
        }
        st
      }

      val (recipeIngredients: IndexedSeq[Seq[Int]], recipeResult: IndexedSeq[Int]) = {
        val ra = new Array[IndexedSeq[Int]](n + 1)
        val rb: Array[Int] = new Array[Int](r)
        (0 until r).foreach { i =>
          val arr = io.StdIn.readLine.split(" ").map(_.toInt)
          ra(i) = arr.view(1, arr.length - 1)
          rb(i) = arr.last
        }

        (ra: IndexedSeq[IndexedSeq[Int]], rb: IndexedSeq[Int])
      }

      val queue = mutable.PriorityQueue.empty(Ordering.by[(Long, Int, Int), Long](_._1).reverse)

      val cost = {
        val c = Array.fill[Long](n + 1, s + 1)(INF)
        for (i <- 1 to n; j <- stones(i)) {
          c(i)(j) = 0
          queue.enqueue((0L, i, j))
        }
        c
      }

      while (queue.nonEmpty) {
        val (d, u, s) = queue.dequeue()

        if (cost(u)(s) == d) {

          // relax neighbors
          graph(u).foreach { v =>
            if (cost(v)(s) > d + 1) {
              cost(v)(s) = d + 1
              queue.enqueue((d + 1, v, s))
            }
          }

          // relax recipes
          (0 until r) foreach { i =>
            if (recipeIngredients(i).contains(s)) {
              val c = recipeIngredients(i).foldLeft(0L)(_ + cost(u)(_))
              if (cost(u)(recipeResult(i)) > c) {
                cost(u)(recipeResult(i)) = c
                queue.enqueue((c, u, recipeResult(i)))
              }
            }
          }
        }
      }

      val i = (1 to n).minBy(cost(_)(1))
      val ans = cost(i)(1)

      printFormattedOutput(t, if (ans >= INF) -1 else ans)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
