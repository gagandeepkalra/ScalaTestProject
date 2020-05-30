package codeforces.contests._1359

/*
https://codeforces.com/contest/1359/problem/C

[Precision] [Linear Equation]

we use BigDecimal and check for the value found and +-1 off of it

 */
object MixingWater {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(h, c, t) = io.StdIn.readLine.split(" ").map(_.toLong)

      println {
        if (t >= h) 1
        else if (t <= (h + c) / 2.0) 2
        else {
          def nearest(y: Long): BigDecimal = (BigDecimal((y + 1) * h + y * c) / BigDecimal(2.0 * y + 1) - BigDecimal(t)).abs

          def solve(y: Long): (BigDecimal, Long) = (nearest(y), 2 * y + 1)

          val y = (t - h) / (h + c - 2 * t)

          val ls = List(solve(y - 1), solve(y), solve(y + 1))

          val min = ls.minBy(_._1)._1

          ls.filter(_._1 == min).minBy(_._2)._2
        }
      }
    }
  }
}
