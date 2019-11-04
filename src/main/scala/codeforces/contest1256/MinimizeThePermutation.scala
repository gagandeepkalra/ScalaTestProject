package codeforces.contest1256

/*
https://codeforces.com/contest/1256/problem/B
 */
object MinimizeThePermutation {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val ls = io.StdIn.readLine().split(" ").map(_.toInt).toList

      def loop(given: List[Int], acc: List[Int] = Nil): List[Int] = {
        if (given.isEmpty) acc
        else {
          val min = given.min

          val (left, right) = given.span(_ != min)

          if (left.isEmpty)
            loop(given.tail, acc :+ given.head)
          else
            loop(left.last +: right.tail, acc ++ (right.head +: left.dropRight(1)))
        }
      }

      println {
        loop(ls).mkString(" ")
      }

    }

  }

}
