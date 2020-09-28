package codeforces.contests._1426

/*
https://codeforces.com/contest/1426/problem/A

Simple arithmetic with divisibility check
 */
object FloorNumber {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, x) = io.StdIn.readLine.split(" ").map(_.toLong)

      val result = {
        if (n <= 2) 1
        else {
          1 + (n - 2) / x + (if ((n - 2) % x != 0) 1 else 0)
        }
      }
      println(result)
    }
  }
}
