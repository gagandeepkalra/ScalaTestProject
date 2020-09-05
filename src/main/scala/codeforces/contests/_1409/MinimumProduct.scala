package codeforces.contests._1409

/*
https://codeforces.com/contest/1409/problem/B

equalise best to maximise
 */
object MinimumProduct {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(a, b, x, y, n) = io.StdIn.readLine.split(" ").map(_.toInt)

      val (f, s) = {
        val aD = (a - n) max x
        val bD = (b - n) max y

        if (aD <= bD) (aD, (b - (n - (a - aD))) max y)
        else ((a - (n - (b - bD))) max x, bD)
      }

      println(f.toLong * s.toLong)
    }
  }
}
