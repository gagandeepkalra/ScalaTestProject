package codeforces.contests._1359

/*
https://codeforces.com/contest/1359/problem/A

the first guy takes all and then we divide the rest equally amongst remaining

 */
object BerlandPoker {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

      val each = n / k

      val first = each min m

      val remaining = m - first

      val second = remaining / (k - 1) + (if (remaining % (k - 1) != 0) 1 else 0)

      println {
        if (first == second) 0 else first - second
      }
    }
  }


}
