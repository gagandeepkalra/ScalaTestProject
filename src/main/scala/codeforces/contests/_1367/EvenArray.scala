package codeforces.contests._1367

/*
https://codeforces.com/contest/1367/problem/B
 */
object EvenArray {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val _ = io.StdIn.readInt
      val arr = io.StdIn.readLine.split(" ").map(_.toInt).zipWithIndex

      val evenOdd = arr.count { case (i, v) => i % 2 != v % 2 && i % 2 == 1 }
      val oddEven = arr.count { case (i, v) => i % 2 != v % 2 && i % 2 == 0 }

      println {
        if (evenOdd == oddEven) evenOdd else -1
      }
    }
  }

}
