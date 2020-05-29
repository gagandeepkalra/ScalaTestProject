package codeforces.contests._1359

/*
https://codeforces.com/contest/1359/problem/B

[Array]

each row can be solved separately then summed, we then compare if it makes sense to use a 1x2 or 1x1 tile

 */
object NewTheatreSquare {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, _, t1, t2) = io.StdIn.readLine.split(" ").map(_.toInt)

      val ans = (1 to n).foldLeft(0) { (acc, _) =>

        val chars = io.StdIn.readLine.toList

        @scala.annotation.tailrec
        def loop(input: List[Char], result: Int = 0): Int = {
          if (input.isEmpty) result
          else {
            val (dots, tail) = input.span(_ == '.')

            val size = dots.length

            val current = if (t1 * 2 <= t2) size * t1 else (size / 2) * t2 + (if ((size & 1) == 1) t1 else 0)

            loop(tail.dropWhile(_ == '*'), result + current)
          }

        }

        acc + loop(chars)
      }

      println {
        ans
      }
    }
  }
}
