package hackerrank

/*
https://www.hackerrank.com/challenges/johnland/problem

[Minimum Spanning Tree]

Given edge weights are unique, we can consider a reduced spanning graph instead
After that, each edge will be counted `nodes to the left * nodes to the right side` times
 */
object RoadsInHackerLand {
  type Graph = Array[List[(Int, Int)]]
  type Edge = (Int, Int, Int)

  def minimumSpanningGraph(n: Int, edges: List[Edge]): Graph = {
    val graph = Array.fill[List[(Int, Int)]](n + 1)(Nil)

    val parent: Array[Int] = {
      val arr = new Array[Int](n + 1)
      (1 to n).foreach { i =>
        arr(i) = i
      }
      arr
    }

    def findParent(i: Int): Int =
      if (i == parent(i))
        i
      else {
        parent(i) = findParent(parent(i)) // path compression
        parent(i)
      }


    def tryInclude(l: Int, r: Int): Boolean = {
      val lp = findParent(l)
      val rp = findParent(r)

      if (lp != rp) {
        parent(rp) = lp
        true
      } else false
    }

    edges.sortBy(_._3).foreach { case (l, r, w) =>
      if (tryInclude(l, r)) {
        graph(l) = (r, w) :: graph(l)
        graph(r) = (l, w) :: graph(r)
      }
    }

    graph
  }

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val edges = (1 to m).foldLeft(List.empty[(Int, Int, Int)]) { (acc, _) =>
      val Array(x, y, w) = io.StdIn.readLine.split(" ").map(_.toInt)
      (x, y, w) :: acc
    }

    val graph: Graph = minimumSpanningGraph(n, edges)

    val resultBits = {
      val pow2Times = new Array[Long](2 * m)

      def dfs(parent: Int, node: Int): Long =
        graph(node).foldLeft(1L) { case (acc, (c, w)) =>
          if (c != parent) {
            val count = dfs(node, c)
            pow2Times(w) += (n - count) * count
            acc + count
          } else acc
        }

      dfs(0, 1)

      @scala.annotation.tailrec
      def foreachSetBit(value: Long, offset: Int = 0)(implicit i: Int): Unit = {
        if (value != 0) {
          if ((value & 1) == 1) {
            pow2Times(i + offset) += 1
          }
          foreachSetBit(value >> 1, offset + 1)
        }
      }

      (0 until 2 * m).foreach { i =>
        val times = pow2Times(i)

        pow2Times(i) = 0
        foreachSetBit(times)(i)
      }

      pow2Times
    }

    println {
      resultBits.mkString.reverse.dropWhile(_ == '0')
    }
  }
}
