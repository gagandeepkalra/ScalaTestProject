package google.kickstart._2020.H

object ReType {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k, s) = io.StdIn.readLine.split(" ").map(_.toLong)

      val result = {
        {
          k - 1 + 1 + n
        } min {
          k - 1 + k - s + n - s + 1
        }
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
