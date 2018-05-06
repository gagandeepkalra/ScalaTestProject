object SherlockAndTheMaze {

  // Lesson learnt, use "LONG"
  def main(args: Array[String]): Unit = {

    val MOD = 1000000007
    val arr = Array.ofDim[Long](101, 101, 101, 2)

    // 0 -> move right , 1-> move down

    for {
      i <- 1 to 100
      j <- 1 to 100
      k <- 0 to 100
      l <- 0 to 1
    } {

      (i, j, k, l) match {

        case (_, 1, 0, 1) => arr(i)(1)(0)(1) = 1
        case (_, 1, _, 1) => arr(i)(1)(k)(1) = 0

        case (1, _, 0, 0) => arr(1)(j)(0)(0) = 1
        case (1, _, _, 0) => arr(1)(j)(k)(0) = 0

        case (_, _, _, 0) => arr(i)(j)(k)(0) = (if (j > 0) arr(i)(j - 1)(k)(0) + (if (k >= 1) arr(i)(j - 1)(k - 1)(1) else 0) else 0) % MOD
        case (_, _, _, 1) => arr(i)(j)(k)(1) = (if (i > 0) arr(i - 1)(j)(k)(1) + (if (k >= 1) arr(i - 1)(j)(k - 1)(0) else 0) else 0) % MOD
      }
    }

    (1 to io.StdIn.readInt()).foreach(_ => {
      val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

      if (n == 1 && m == 1) println(1)
      else println((0 to k).foldLeft[Long](0)((res, x) => (res + arr(n)(m)(x).sum) % MOD))
    })

  }

}
