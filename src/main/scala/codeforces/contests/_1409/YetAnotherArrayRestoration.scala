package codeforces.contests._1409

/*
https://codeforces.com/contest/1409/problem/C
 */
object YetAnotherArrayRestoration {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(n, x, y) = io.StdIn.readLine.split(" ").map(_.toInt)

      val (first, last, diff) = (0 to n - 2).map { i =>
        if ((y - x) % (i + 1) == 0) {
          val d = (y - x) / (i + 1)

          @scala.annotation.tailrec
          def loop(j: Int, r: Int = n - 2 - i): Int = if (j <= 0) j + d else if (r == 0) j else loop(j - d, r - 1)

          val f = loop(x)
          (f, f + (n - 1) * d, d)
        } else (Int.MaxValue, Int.MaxValue, Int.MaxValue)
      }.minBy(_._2)

      println {
        (first to last by diff).mkString(" ")
      }
    }
  }
}
