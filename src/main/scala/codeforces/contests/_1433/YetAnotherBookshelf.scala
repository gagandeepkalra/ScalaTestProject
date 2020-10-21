package codeforces.contests._1433

/*
https://codeforces.com/contest/1433/problem/B
 */
object YetAnotherBookshelf {

  import scala.annotation.tailrec

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val digits = io.StdIn.readLine.split(" ").map(_.toInt)

      @tailrec
      def loop(ls: List[Int], acc: List[Int] = Nil): List[Int] = {
        if (ls.isEmpty) acc else {
          val afterOnesDropped = ls.dropWhile(_ == 1)
          if (afterOnesDropped.isEmpty) acc else {
            val (zeroes, afterZeroesDropped) = afterOnesDropped.span(_ == 0)
            if (afterZeroesDropped.isEmpty) acc else {
              loop(afterZeroesDropped, zeroes.length :: acc)
            }
          }
        }
      }

      val zeroesInThMiddle = loop(digits.toList.dropWhile(_ == 0))

      println {
        zeroesInThMiddle.sum
      }
    }
  }
}
