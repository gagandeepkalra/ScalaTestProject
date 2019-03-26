package codeforces.contest1099

object SquaresAndSegments {
  def main(args: Array[String]): Unit = {
    val x = io.StdIn.readInt()

    val n = math.sqrt(x).floor.toInt

    if (n * n == x) println(2 * n)
    else if (n * n < x && x <= n * n + n) println(2 * n + 1)
    else if (n * n + n < x && x <= n * n + 2 * n) println(2 * n + 2)
    else println(2 * n + 2)

  }

}
