package codeforces.contests._1462

/*
https://codeforces.com/contest/1462/problem/A
 */
object FavoriteSequence {

  import scala.annotation.tailrec

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      @tailrec
      def loop(i: Int, j: Int, acc: List[Int] = Nil): List[Int] = {
        if (i > j)
          acc.reverse
        else if (i == j)
          (arr(i) :: acc).reverse
        else
          loop(i + 1, j - 1, arr(j) :: arr(i) :: acc)
      }

      println {
        loop(0, n - 1).mkString(" ")
      }
    }
  }
}
