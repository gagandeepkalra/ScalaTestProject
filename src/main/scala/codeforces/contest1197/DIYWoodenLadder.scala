package codeforces.contest1197

/*
https://codeforces.com/contest/1197/problem/A
 */
object DIYWoodenLadder {
  def main(args: Array[String]): Unit = {
    println((1 to io.StdIn.readInt).map { _ =>
      val n = io.StdIn.readInt()
      val seq = io.StdIn.readLine().split(" ").map(_.toInt).sorted

      if (seq.length > 2) {
        seq(n - 2) - 1 min n - 2
      } else 0


    }.mkString("\n"))
  }
}
