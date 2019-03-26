package codeforces.contest1139

object Chocolates {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val arr = io.StdIn.readLine().split(" ").map(_.toInt)

    println(arr.foldRight((Int.MaxValue, 0l)) { case (current, (maximum: Int, sum: Long)) =>
      val chocolates = (current min (maximum - 1)) max 0
      (chocolates, sum + chocolates)
    }._2)
  }
}
