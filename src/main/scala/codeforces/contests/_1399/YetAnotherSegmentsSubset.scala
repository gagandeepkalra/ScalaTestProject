package codeforces.contests._1399

import scala.collection.mutable.ArrayBuffer

/*
https://codeforces.com/contest/1399/problem/F

[Dynamic Programming]

The problem is recursive in nature, we memoize intermediate results

We sort the input based on r, otherwise larger/later l comes first

We find wholly inclusive sub-segments within each segment (recursive nature comes from here)

Then, dp(i) = result considering the first i pairs
            = dp(i-1) we ignore the last pair or dp(j) + solve(sub-segments(i)) where pair(j).r < pair(i).l

Mix in some binary search and we have an efficient solution.
 */
object YetAnotherSegmentsSubset {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val input: Array[Array[Int]] = new Array[Array[Int]](n) // l and r
      input.indices.foreach(input(_) = io.StdIn.readLine.split(" ").map(_.toInt))

      val pairs = {
        val sorted = input.sortWith { (first, second) =>
          val fr = first(1)
          val sr = second(1)
          if (fr == sr) {
            val fl = first(0)
            val sl = second(0)
            fl >= sl
          } else fr < sr
        }

        val result = new Array[(Int, Int, Int)](n)
        (0 until n).foreach(i => result(i) = (sorted(i)(0), sorted(i)(1), i))
        result
      }

      val subSegments: Array[Array[(Int, Int, Int)]] = {
        val subs = Array.fill[ArrayBuffer[(Int, Int, Int)]](n)(ArrayBuffer.empty)

        for (i <- 0 until n) {
          val fi = pairs(i)._1
          var j = i - 1
          while (j >= 0 && pairs(j)._2 >= fi) {
            if (pairs(j)._1 >= fi) subs(i) += pairs(j)
            j -= 1
          }
        }

        subs.map(_.toArray.reverse)
      }

      val cache = Array.fill[Int](n + 1)(-1) // sub-segment result for each interval

      def solveAndCacheAtIdx(seq: Array[(Int, Int, Int)], idx: Int): Int = {
        if (seq.isEmpty) 0
        else if (cache(idx) != -1) cache(idx)
        else {
          val dp = new Array[Int](seq.length)
          for (i <- seq.indices) {
            val pos = seq(i)._3
            dp(i) = 1 + solveAndCacheAtIdx(subSegments(pos), pos) + {

              val key = seq(i)._1

              var l = 0
              var j = i - 1

              while (l <= j) {
                val m = (l + j) / 2
                if (seq(m)._2 >= key) j = m - 1
                else l = m + 1
              }

              if (j >= 0) dp(j) else 0
            }

            dp(i) = dp(i) max (if (i > 0) dp(i - 1) else 0)
          }
          val result = dp.max
          cache(idx) = result
          result
        }
      }

      println(solveAndCacheAtIdx(pairs, n))
    }
  }
}
