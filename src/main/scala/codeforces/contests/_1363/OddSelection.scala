package codeforces.contests._1363

/*
https://codeforces.com/contest/1363/problem/A

odd + odd = even
even + odd = odd
 */
object OddSelection {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, x) = io.StdIn.readLine.split(" ").map(_.toInt)

      val odds = io.StdIn.readLine.split(" ").map(_.toInt).count(_ % 2 == 1)
      val evens = n - odds

      if (evens == 0) {
        println(if (x % 2 == 1) "Yes" else "No")
      } else {
        val oddsNeeded = { // 5 then need 5 odds, if 6 then need 7 odds, if evens == 0
          val r = (x - evens) max 0
          r + (if (r % 2 == 0) 1 else 0)
        }

        val available = oddsNeeded <= odds

        println(if (available) "Yes" else "No")
      }
    }
  }
}
