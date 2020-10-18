package google.kickstart._2020.G

/*
https://codingcompetitions.withgoogle.com/kickstart/round/00000000001a0069/0000000000414a23

Count across all backward leaning diagonals, find max
 */
object MaximumCoins {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()

      val matrix: Array[Array[Int]] = {
        val arr = Array.ofDim[Int](n, n)
        (0 until n).foreach(arr(_) = io.StdIn.readLine().split(" ").map(_.toInt))
        arr
      }

      val result = {
        (0 until n).foldLeft(Long.MinValue) { (acc, row) =>
          acc max {
            (row until n).foldLeft((0L, 0)) { case ((sum, c), r) => (sum + matrix(r)(c), c + 1) }._1
          }
        }
      } max {
        (1 until n).foldLeft(Long.MinValue) { (acc, col) =>
          acc max {
            (col until n).foldLeft((0L, 0)) { case ((sum, r), c) => (sum + matrix(r)(c), r + 1) }._1
          }
        }
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
