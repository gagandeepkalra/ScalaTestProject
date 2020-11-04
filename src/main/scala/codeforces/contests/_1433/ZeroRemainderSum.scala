package codeforces.contests._1433

/*
https://codeforces.com/contest/1433/problem/F

[Dynamic Programming] [Subset sum]

You can choose no more than ⌊𝑚/2⌋ elements in each row. Your task is to choose these elements in such a way that their
sum is divisible by 𝑘 and this sum is the maximum.

dp(i)(j)(c)(r) = ith row, jth column, having included c elements from the ith row, with remainder r, the max sum achievable

We do this expansion style, this would mean that when we reach a cell we update those forward next affected by it.
Row transition, when at the last element of a row, we update the next dp(i+1)(0)(0)(r)

This problem differs from subset sum in that we don't the full value for the last dimension instead only the remainder
 */
object ZeroRemainderSum {
  def main(args: Array[String]): Unit = {
    val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val matrix = {
      val matrix = new Array[Array[Int]](n)
      (0 until n).foreach(matrix(_) = io.StdIn.readLine.split(" ").map(_.toInt))
      matrix
    }

    val dp = Array.fill(n + 1, m, m / 2 + 1, k)(Int.MinValue)

    dp(0)(0)(0)(0) = 0

    for {
      i <- 0 until n
      j <- 0 until m
      cM = ((m / 2) min (j + 1)) + 1
      c <- 0 until cM
      r <- 0 until k
    } {

      if (dp(i)(j)(c)(r) != Int.MinValue) {

        if (j < m - 1) {
          dp(i)(j + 1)(c)(r) = dp(i)(j + 1)(c)(r) max dp(i)(j)(c)(r)

          if (c + 1 < cM) {
            val rr = (r + matrix(i)(j)) % k
            dp(i)(j + 1)(c + 1)(rr) = dp(i)(j + 1)(c + 1)(rr) max {
              dp(i)(j)(c)(r) + matrix(i)(j)
            }
          }
        } else { // next row
          dp(i + 1)(0)(0)(r) = dp(i + 1)(0)(0)(r) max dp(i)(j)(c)(r)

          val rr = (r + matrix(i)(j)) % k
          if (c + 1 < cM) {
            dp(i + 1)(0)(0)(rr) = dp(i + 1)(0)(0)(rr) max {
              dp(i)(j)(c)(r) + matrix(i)(j)
            }
          }
        }
      }
    }

    println(dp(n)(0)(0)(0) max 0)

  }
}
