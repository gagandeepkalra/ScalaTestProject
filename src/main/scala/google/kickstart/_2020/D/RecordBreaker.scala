package google.kickstart._2020.D

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff08/

sequential left to right maintain count and greatest seen/max
 */
object RecordBreaker {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      val result = arr.indices.foldLeft((0, -1)) { case ((acc, greatestSeen), i) =>
        val v = arr(i)
        if (v > greatestSeen && (i == n - 1 || v > arr(i + 1))) (acc + 1, v) else (acc, v max greatestSeen)
      }._1

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
