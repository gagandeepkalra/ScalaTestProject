package codeforces.contests._1426

/*
https://codeforces.com/contest/1426/problem/C

Maxima achieved at square root.
 */
object IncreaseAndCopy {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()

      val sqrt = math.sqrt(n).toInt

      val result = {
        if (sqrt * sqrt == n) {
          sqrt - 1 + sqrt - 1
        } else {
          sqrt - 1 + n / sqrt + (if (n % sqrt == 0) -1 else 0)

        }
      }
      println(result)
    }
  }
}
