package codeforces.contests._1353

/*
https://codeforces.com/contest/1353/problem/A
 */
object MostUnstableArray {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
      println {
        n match {
          case 1 => 0
          case 2 => m
          case _ => 2L * m
        }
      }
    }
  }

}
