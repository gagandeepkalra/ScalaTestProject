package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/A
 */
object RequiredRemainder {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val Array(x, y, n) = io.StdIn.readLine.split(" ").map(_.toInt)

      val maxN = {
        val r = n % x
        val mN = n - r + y
        if (mN > n) mN - x else mN
      }

      println(maxN)
    }
  }
}
