package GoogleCodeJam2019Qualifications

object YouCanGoYourOwnWay {
  def main(args: Array[String]): Unit = {
    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {
      val n = io.StdIn.readInt()

      val path = io.StdIn.readLine()

      println(s"Case #$t: ${path.map(s => if (s == 'S') 'E' else 'S')}")
    }
  }
}
