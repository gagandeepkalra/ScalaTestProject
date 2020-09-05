package codeforces.contests._1409

/*
https://codeforces.com/contest/1409/problem/A
 */
object YetAnotherTwoIntegersProblem {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
      println {
        val d = (a - b).abs

        if (d % 10 == 0) d / 10 else d / 10 + 1
      }
    }
  }
}
