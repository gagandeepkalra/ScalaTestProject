package google.kickstart._2019.F

import scala.collection.mutable.ListBuffer

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edc/000000000018666b

[Graph Theory: Traversal]

Given graph is a tree, we calculate (Selected, Partial, NotSelected) state value for each node

When available for all children of node i:
Selected = Sum (for every child j, max(we can either select that child or partially select or not select further (but add w(j)) for inclusion))
NotSelected = Sum (for every child j, max(we can either partially select or not select further for inclusion))
Partial = for i to be partial at least one child hast be total (selected), Sum (for every child j, max(we can either select that child or partially select or not select further for inclusion))
 */
object SpectatingVillages {
  type Graph[T] = Array[List[T]]

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val weight: Array[Int] = 0 +: io.StdIn.readLine.split(" ").map(_.toInt)

      val graph: Graph[Int] = {
        val arr = Array.fill[List[Int]](n + 1)(List.empty[Int])
        (1 until n).foreach { _ =>
          val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
          arr(x) = y :: arr(x)
          arr(y) = x :: arr(y)
        }
        arr
      }

      def dfs(parent: Int, i: Int): (Long, Long, Long) = { // (Selected, Partial, NotSelected)
        var s, fullP = weight(i).toLong
        var n = 0L
        var pBuffer = ListBuffer.empty[(Long, Long, Long)]

        graph(i).foreach { j =>
          if (j != parent) {
            val r@(sj, pj, nj) = dfs(i, j)
            s += (sj max pj max (nj + weight(j)))
            fullP += (sj max pj max nj)
            n += (pj max nj)

            pBuffer.append(r)
          }
        }

        val p = if (pBuffer.isEmpty) Long.MinValue else {
          pBuffer.foldLeft(Long.MinValue) { case (acc, (sj, pj, nj)) =>
            val current = sj max pj max nj
            acc max (fullP - current + sj)
          }
        }

        (s, p, n)
      }

      val (s1, p1, n1) = dfs(0, 1)

      printFormattedOutput(t, s1 max p1 max n1)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
