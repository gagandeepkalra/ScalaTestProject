package codeforces.contest1213

/*
 * Disjoint Set
 * https://codeforces.com/contest/1213/problem/G
 */
object PathQueries {

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
    val edgesList = (1 until n)
      .map { _ =>
        val Array(l, r, w) = io.StdIn.readLine.split(" ").map(_.toInt)
        (l min r, l max r, w)
      }
      .sortBy(_._3)
      .toList

    val parent = new Array[Int](n + 1)
    val setSize = new Array[Long](n + 1)

    def findParent(i: Int): Int =
      if (i == parent(i))
        i
      else {
        parent(i) = findParent(parent(i)) // path compression
        parent(i)
      }

    (1 to n).foreach { i =>
      parent(i) = i
      setSize(i) = 1
    }

    @scala.annotation.tailrec
    def loop(edges: List[(Int, Int, Int)],
             max: Int,
             result: Long): (Long, List[(Int, Int, Int)]) =
      edges match {
        case ::(_ @(l, r, w), tl) if w <= max =>
          val lp = findParent(l)
          val rp = findParent(r)

          val newResult = result + setSize(lp) * setSize(rp)

          setSize(lp) += setSize(rp)
          parent(rp) = lp

          loop(tl, max, newResult)

        case _ => (result, edges)
      }

    val result = new Array[Long](m)

    io.StdIn.readLine
      .split(" ")
      .map(_.toInt)
      .zipWithIndex
      .sortBy(_._1)
      .foldLeft((0l, edgesList)) {
        case ((acc, ls), (q, i)) =>
          val (r, remaining) = loop(ls, q, acc)
          result(i) = r
          (r, remaining)
      }

    println(result.mkString(" "))
  }

}
