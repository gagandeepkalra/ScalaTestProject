package GoogleCodeJam2019RoundG

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050e02/000000000018fd0d
 */
object BookReading {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt()) {
      val Array(n, _, _) = io.StdIn.readLine.split(" ").map(_.toInt)

      val missing = io.StdIn.readLine.split(" ").map(_.toInt)

      val multiples = io.StdIn.readLine.split(" ").map(_.toInt)

      val reader = Array.fill[Int](n + 1)(1)

      missing.foreach(reader(_) = 0)

      def calculatePagesCanRead(start: Int): Int = {
        var count = 0
        for (i <- start to n by start) count += reader(i)
        count
      }

      val result: Map[Int, Int] = multiples.toSet[Int].map(i => i -> calculatePagesCanRead(i)).toMap

      println {
        s"Case #$t: ${multiples.map(result).sum}"
      }

    }

  }

}
