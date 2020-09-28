package codeforces.contests._1426

/*
https://codeforces.com/contest/1426/problem/B

Pattern finding.
 */
object SymmetricMatrix {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

      val result = (1 to n).map { _ =>
        val row0, row1 = io.StdIn.readLine.split(" ").map(_.toInt)
        row0(1) == row1(0)
      }.exists(identity) && m % 2 == 0

      println(if (result) "YES" else "NO")
    }
  }
}
