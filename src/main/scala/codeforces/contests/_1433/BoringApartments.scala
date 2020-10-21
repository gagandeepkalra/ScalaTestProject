package codeforces.contests._1433

/*
https://codeforces.com/contest/1433/problem/A
 */
object BoringApartments {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val digit = n % 10
      val size = n.toString.length

      println {
        ((digit - 1) max 0) * 10 + (1 to size).sum
      }
    }
  }
}
