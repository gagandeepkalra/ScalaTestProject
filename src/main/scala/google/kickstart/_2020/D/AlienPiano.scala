package google.kickstart._2020.D

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff08/0000000000387174

[Dynamic Programming]

For each pitch value starting from right to left, we calculate the minimum breakages for each selection,
afterwards we use this result to compute for next (i-1).

dp(0)(j) -> choosing A at jth position
dp(1)(j) -> choosing B at jth position
dp(2)(j) -> choosing C at jth position
dp(3)(j) -> choosing D at jth position

 */
object AlienPiano {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      val dp: Array[Array[Int]] = Array.ofDim[Int](4, n)

      for (j <- n - 2 to 0 by -1) {
        dp(0)(j) = { // A
          if (arr(j) < arr(j + 1))
            (dp(0)(j + 1) + 1) min dp(1)(j + 1) min dp(2)(j + 1) min dp(3)(j + 1)
          else if (arr(j) == arr(j + 1))
            dp(0)(j + 1) min ((dp(1)(j + 1) min dp(2)(j + 1) min dp(3)(j + 1)) + 1)
          else // arr(j) > arr(j + 1)
            (dp(0)(j + 1) min dp(1)(j + 1) min dp(2)(j + 1) min dp(3)(j + 1)) + 1
        }

        dp(1)(j) = { // B
          if (arr(j) < arr(j + 1))
            (dp(0)(j + 1) + 1) min (dp(1)(j + 1) + 1) min dp(2)(j + 1) min dp(3)(j + 1)
          else if (arr(j) == arr(j + 1))
            (dp(0)(j + 1) + 1) min dp(1)(j + 1) min (dp(2)(j + 1) + 1) min (dp(3)(j + 1) + 1)
          else // arr(j) > arr(j + 1)
            dp(0)(j + 1) min (dp(1)(j + 1) + 1) min (dp(2)(j + 1) + 1) min (dp(3)(j + 1) + 1)
        }

        dp(2)(j) = { // C
          if (arr(j) < arr(j + 1))
            (dp(0)(j + 1) + 1) min (dp(1)(j + 1) + 1) min (dp(2)(j + 1) + 1) min dp(3)(j + 1)
          else if (arr(j) == arr(j + 1))
            (dp(0)(j + 1) + 1) min (dp(1)(j + 1) + 1) min dp(2)(j + 1) min (dp(3)(j + 1) + 1)
          else // arr(j) > arr(j + 1)
            dp(0)(j + 1) min dp(1)(j + 1) min (dp(2)(j + 1) + 1) min (dp(3)(j + 1) + 1)
        }

        dp(3)(j) = { // D
          if (arr(j) < arr(j + 1))
            (dp(0)(j + 1) + 1) min (dp(1)(j + 1) + 1) min (dp(2)(j + 1) + 1) min (dp(3)(j + 1) + 1)
          else if (arr(j) == arr(j + 1))
            (dp(0)(j + 1) + 1) min (dp(1)(j + 1) + 1) min (dp(2)(j + 1) + 1) min dp(3)(j + 1)
          else // arr(j) > arr(j + 1)
            dp(0)(j + 1) min dp(1)(j + 1) min dp(2)(j + 1) min (dp(3)(j + 1) + 1)
        }
      }
      val result = dp(0)(0) min dp(1)(0) min dp(2)(0) min dp(3)(0)

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }

}
