package codeforces.contest1175

/*
https://codeforces.com/contest/1175/problem/C
 */
object Electrification {
  def main(args: Array[String]): Unit = {
    println((1 to io.StdIn.readInt).map { _ =>
      val Array(n, k) = io.StdIn.readLine().split(" ").map(_.toInt)
      val seq = io.StdIn.readLine().split(" ").map(_.toInt)

      (0 until n - k).map {
        i =>
          val res = (seq(i + k) + seq(i)) / 2
          (res, (seq(i + k) - res) max (res - seq(i)))
      }.minBy(_._2)._1

    }.mkString("\n"))
  }
}
