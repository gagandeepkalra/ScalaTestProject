package hackerrank

import java.lang.Math.{ceil, floor}

/*
https://www.hackerrank.com/challenges/clique/problem

Clique
 */
object Clique {

  def turan(n: Int, r: Int): Int = {
    val c = ceil(n * 1.0 / r).toInt
    val f = floor(n * 1.0 / r).toInt
    n * (n - 1) / 2 - (n % r) * (c * (c - 1)) / 2 - (r - n % r) * f * (f - 1) / 2
  }

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

      @scala.annotation.tailrec
      def binarySearch(l: Int, r: Int): Int = {
        if (l > r)
          l
        else {
          val mid = (l + r) / 2
          val edges = turan(n, mid)

          if (edges == m)
            mid
          else if (edges < m)
            binarySearch(mid + 1, r)
          else
            binarySearch(l, mid - 1)
        }
      }

      println(binarySearch(1, n))
    }
  }

}
