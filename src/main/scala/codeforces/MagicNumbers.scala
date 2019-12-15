package codeforces

/*
https://codeforces.com/contest/628/problem/D

Digit DP
 */
object MagicNumbers {

  val mod: Int = 1000000000 + 7

  def isGood(s: String, m: Int, d: Char): Boolean =
    s.foldLeft(0)((rem, c) => (rem * 10 + c - '0') % m) == 0 &&
      s.indices.forall(i => if (i % 2 == 0) s(i) != d else s(i) == d)

  def main(args: Array[String]): Unit = {
    println {

      val Array(m, d) = io.StdIn.readLine().split(" ").map(_.toInt)

      val l, r = io.StdIn.readLine

      val lDigits = l.map(_ - '0')
      val rDigits = r.map(_ - '0')

      val Idx = lDigits.size

      val dp = Array.ofDim[Int](Idx + 1, m, 2) // 0 mean no ceil, 1 means ceil

      def solve(digits: Seq[Int]): Int = {

        dp(Idx)(0)(0) = 1
        dp(Idx)(0)(1) = 1

        for (idx <- Idx - 1 to 0 by -1; r <- 0 until m) {
          if (idx % 2 == 1) {
            dp(idx)(r)(0) = dp(idx + 1)((r * 10 + d) % m)(0)
            dp(idx)(r)(1) =
              if (d < digits(idx))
                dp(idx + 1)((r * 10 + d) % m)(0)
              else if (d == digits(idx))
                dp(idx + 1)((r * 10 + d) % m)(1)
              else 0

          } else {

            dp(idx)(r)(0) = (0 to 9)
              .filter(_ != d)
              .foldLeft(0)(
                (acc, i) => (acc + dp(idx + 1)((r * 10 + i) % m)(0)) % mod
              )
            dp(idx)(r)(1) = ((0 until digits(idx))
              .filter(_ != d)
              .foldLeft(0)(
                (acc, i) => (acc + dp(idx + 1)((r * 10 + i) % m)(0)) % mod
              ) + dp(idx + 1)((r * 10 + digits(idx)) % m)(1)) % mod
          }
        }

        dp(0)(0)(1)
      }

      (solve(rDigits) - solve(lDigits) + (if (isGood(l, m, (d + '0').toChar)) 1 else 0) + mod) % mod
    }
  }
}
