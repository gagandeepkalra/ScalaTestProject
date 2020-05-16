package codeforces.contests._1353

/*
https://codeforces.com/contest/1353/problem/F

[Dynamic Programming]

each path must have an anchor, a minimum value that governs the path result
for each possible anchor, we find result and take minimum
 */
object DecreasingHeights {

  val dp: Array[Array[Long]] = Array.ofDim[Long](101, 101)

  def reset(n: Int, m: Int): Unit = for (i <- 0 until n; j <- 0 until m) dp(i)(j) = Long.MaxValue

  def main(args: Array[String]): Unit = {
    println {
      {
        for (_ <- 1 to io.StdIn.readInt) yield {
          val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
          val grid = (0 until n).map(_ => io.StdIn.readLine.split(" ").map(_.toLong))

          val result = {
            var r = Long.MaxValue

            for (ii <- 0 until n; jj <- 0 until m) {
              val base: Long = grid(ii)(jj) - ii - jj
              if (grid(0)(0) >= base) {
                reset(n, m)
                dp(0)(0) = grid(0)(0) - base

                for (i <- 0 until n; j <- 0 until m) if (dp(i)(j) != Long.MaxValue) {
                  if (i + 1 < n) {
                    val expected = base + i + 1 + j
                    val actual = grid(i + 1)(j)
                    if (actual >= expected)
                      dp(i + 1)(j) = dp(i + 1)(j) min (actual - expected + dp(i)(j))
                  }
                  if (j + 1 < m) {
                    val expected = base + i + j + 1
                    val actual = grid(i)(j + 1)
                    if (actual >= expected)
                      dp(i)(j + 1) = dp(i)(j + 1) min (actual - expected + dp(i)(j))
                  }
                }
                r = r min dp(n - 1)(m - 1)
              }
            }

            r
          }
          result
        }
      }.mkString("\n")
    }
  }
}
