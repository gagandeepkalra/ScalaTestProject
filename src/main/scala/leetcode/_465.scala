package leetcode

/**
 * Splitwise, debt simplification; np complete
 */
object _465 {

  def minTransfers(transactions: Array[Array[Int]]): Int = {
    val size = transactions.map { case Array(l, r, _) => l max r }.max + 1

    val input: Vector[Int] = {
      val arr = new Array[Int](size)
      transactions.foreach {
        case Array(l, r, v) =>
          arr(l) -= v
          arr(r) += v
      }
      arr
    }.toVector

    recur(input.filter(_ != 0))
  }

  private def recur(input: Vector[Int], idx: Int = 0): Int = {
    if (idx >= input.length) 0
    else if (input(idx) == 0) recur(input, idx + 1)
    else {
      val n = input.length

      val result = (idx + 1 until n).collect {
        case i if input(idx) * input(i) < 0 =>
          1 + recur(input.updated(i, input(i) + input(idx)), idx + 1)
      }

      result.min
    }
  }

  def main(args: Array[String]): Unit = {
    val first = Array(Array(0, 1, 10), Array(2, 0, 5))
    val second =
      Array(Array(0, 1, 10), Array(1, 0, 1), Array(1, 2, 5), Array(2, 0, 5))

    val third =
      Array(Array(0, 1, 1),
            Array(1, 2, 1),
            Array(2, 3, 4),
            Array(3, 4, 5),
            Array(4, 0, 1))

    val fourth =
      Array(Array(0, 3, 2), Array(1, 4, 3), Array(2, 3, 2), Array(2, 4, 2))

    println {
      minTransfers(fourth)
    }

  }
}
