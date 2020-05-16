package codeforces.contests._1353

/*
https://codeforces.com/contest/1353/problem/E

[Dynamic Programming]

dp(i) = cost of putting a 1 at ith index and afterwards
      = (cost of putting a 1 at ith index) + costZeroing(i + 1, i + k - 1) + (costOne(i + k) min costZeroing(i + k, n - 1))

final result will be min of (checking each sequence by putting 1 at each i or none at all)
 */
object KPeriodicGarland {
  def main(args: Array[String]): Unit = {
    println {
      {
        for (_ <- 1 to io.StdIn.readInt) yield {
          val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
          val input = io.StdIn.readLine.map(_ - '0')

          val prefixSum = input.scanLeft(0)(_ + _)

          val dp = new Array[Int](n) // cost of putting a 1 at ith index

          def costOne(i: Int): Int = // cost of putting a 1 at ith index
            if (i >= n) 0 else dp(i)

          def costZeroing(l: Int, r: Int): Int = // cost of zeroing between l and r
            if (l >= n || l > r) 0 else prefixSum(n min (r + 1)) - prefixSum(l)

          for (i <- n - 1 to 0 by -1)
            dp(i) = (1 ^ input(i)) + costZeroing(i + 1, i + k - 1) + (costOne(i + k) min costZeroing(i + k, n - 1))

          val i = (0 until n).minBy(i => dp(i) + costZeroing(0, i - 1))
          (dp(i) + costZeroing(0, i - 1)) min costZeroing(0, n - 1)
        }
      }.mkString("\n")
    }
  }
}
