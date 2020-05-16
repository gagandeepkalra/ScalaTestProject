package codeforces.contests._1353

/*
https://codeforces.com/contest/1353/problem/B
 */
object TwoArraysAndSwaps {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

      val a, b = io.StdIn.readLine.split(" ").map(_.toInt).sorted
      println {
        a.slice(k, n).sum + (a.slice(0, k) ++ b.slice(n - k, n)).sorted(Ordering.Int.reverse).take(k).sum
      }
    }
  }
}
