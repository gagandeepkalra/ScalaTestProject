package codeforces.contests._1407

import algorithms.arrays.Stack._

/*
https://codeforces.com/contest/1407/problem/D

[Monotonic Stack]

We have two cases:

ℎ𝑥 ≤ ℎ𝑦. Then, obviously, 𝑦 is the first skyscraper that not lower than 𝑥 (otherwise we have a building that higher than starter, it's contradiction).
ℎ𝑥 > ℎ𝑦. Then, it's easy to see, that 𝑥 is the first skyscraper to the left of 𝑦, that higher than 𝑦 for the same reason.

Dynamic programming to the solution
 */
object DiscreteCentrifugalJumps {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val arr = io.StdIn.readLine.split(" ").map(_.toInt)

    val rightIncreasingView: IndexedSeq[Int] = rightView(arr)
    val rightDecreasingView: IndexedSeq[Int] = rightView(arr)(Ordering.Int.reverse)

    val leftIncreasingView = leftView(arr)
    val leftDecreasingView = leftView(arr)(Ordering.Int.reverse)

    println {
      val dp: Array[Int] = (0 until n).toArray
      dp.indices.foreach { i =>

        // relax dp(i) first
        if (leftIncreasingView(i) != -1)
          dp(i) = dp(i) min (dp(leftIncreasingView(i)) + 1)

        if (leftDecreasingView(i) != -1)
          dp(i) = dp(i) min (dp(leftDecreasingView(i)) + 1)

        // relax the future
        if (rightIncreasingView(i) != -1)
          dp(rightIncreasingView(i)) = dp(rightIncreasingView(i)) min (dp(i) + 1)

        if (rightDecreasingView(i) != -1)
          dp(rightDecreasingView(i)) = dp(rightDecreasingView(i)) min (dp(i) + 1)

        if (i + 1 < n)
          dp(i + 1) = dp(i + 1) min (dp(i) + 1)
      }
      dp(n - 1)
    }

  }

}
