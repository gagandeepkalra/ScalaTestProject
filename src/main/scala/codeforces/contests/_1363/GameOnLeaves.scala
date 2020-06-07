package codeforces.contests._1363

/*
https://codeforces.com/contest/1363/problem/C

simple two player game ordering
 */
object GameOnLeaves {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(n, x) = io.StdIn.readLine.split(" ").map(_.toInt)

      val degree: Array[Int] = {
        val a = new Array[Int](n + 1)
        (1 until n).foreach { _ =>
          val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
          a(x) += 1
          a(y) += 1
        }
        a
      }
      val first = "Ayush"
      val second = "Ashish"

      println {
        if (degree(x) <= 1) first else {
          val maxMoves = n - 2

          if (maxMoves % 2 == 0) first else second
        }
      }
    }
  }
}
