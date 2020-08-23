package google.kickstart._2020.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff47/00000000003bf4ed

Max count of contiguous equal difference elements.
 */
object LongestArithmetic {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      val seq = arr.sliding(2).map { case Array(f, s) => f - s }

      @scala.annotation.tailrec
      def loop(ls: List[Int], result: Int = 0): Int = {
        ls match {
          case Nil => result
          case h :: tail =>
            val newResult = result max (1 + tail.takeWhile(_ == h).size)
            loop(tail.dropWhile(_ == h), newResult)
        }
      }

      printFormattedOutput(t, loop(seq.toList) + 1)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
