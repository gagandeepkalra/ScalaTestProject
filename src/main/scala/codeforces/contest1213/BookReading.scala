package codeforces.contest1213

/*
https://codeforces.com/contest/1213/problem/C
 */
object BookReading {

  val lastDigitToSeq: Map[Int, Seq[Int]] = Map(
    0 -> Seq(0),
    1 -> Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
    2 -> Seq(2, 4, 6, 8, 0),
    3 -> Seq(3, 6, 9, 2, 5, 8, 1, 4, 7, 0),
    4 -> Seq(4, 8, 2, 6, 0),
    5 -> Seq(5, 0),
    6 -> Seq(6, 2, 8, 4, 0),
    7 -> Seq(7, 4, 1, 8, 5, 2, 9, 6, 3, 0),
    8 -> Seq(8, 6, 4, 2, 0),
    9 -> Seq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  )

  def main(args: Array[String]): Unit = {
    println {
      (1 to io.StdIn.readInt())
        .foldLeft(List.empty[Long]) {
          case (ls, _) =>
            val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toLong)

            val lastDigit = (m % 10).toInt
            val seq = lastDigitToSeq(lastDigit)
            val (freq: Int, sum: Int) = (seq.length, seq.sum)

            val count: Long = n / m

            (sum * (count / freq) + seq.take((count % freq).toInt).sum) :: ls
        }
        .reverse
        .mkString("\n")
    }
  }

}
