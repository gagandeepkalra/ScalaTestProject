package codeforces.contests._1353

/*
https://codeforces.com/contest/1353/problem/C
 */
object BoardMoves {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readLong / 2

      println {
        (n * (n + 1) * (2 * n + 1) * 8) / 6
      }
    }
  }
}
