package codeforces.contest1256

/*
https://codeforces.com/contest/1256/problem/A
 */
object PaymentWithoutChange {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val Array(a, b, n, s) = io.StdIn.readLine().split(" ").map(_.toLong)

      println {
        if (b + (n * (s / n) min a * n) >= s) "YES" else "NO"
      }
    }
  }

}
