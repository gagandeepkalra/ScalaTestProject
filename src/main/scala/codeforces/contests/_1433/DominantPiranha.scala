package codeforces.contests._1433

/*
https://codeforces.com/contest/1433/problem/C

We find any max guy which has a non max neighbour
 */
object DominantPiranha {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val digits = io.StdIn.readLine.split(" ").map(_.toInt)

      val max = digits.max

      val result = (0 until n).filter(digits(_) == max).find { i =>
        (i - 1 >= 0 && digits(i - 1) != max) || (i + 1 < n && digits(i + 1) != max)
      }.getOrElse(-2) + 1

      println {
        result
      }
    }
  }
}
