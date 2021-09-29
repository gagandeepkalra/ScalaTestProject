package codeforces.contests._1462

/*
https://codeforces.com/contest/1462/problem/B
 */
object LastYearsSubstring {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val _ = io.StdIn.readInt()

      val s = io.StdIn.readLine.trim

      val p1 = "^\\d*2020$".r
      val p2 = "^2\\d*020$".r
      val p3 = "^20\\d*20$".r
      val p4 = "^202\\d*0$".r
      val p5 = "^2020\\d*$".r

      val result = {
        s match {
          case p1() | p2() | p3() | p4() | p5() => true
          case _                                => false
        }
      }

      println {
        if (result) "YES" else "NO"
      }
    }
  }
}
