package codeforces.contests._1409

/*
https://codeforces.com/contest/1409/problem/F

[Dynamic Programming]

we calculate dp(i)(j)(k) =  maximum subsequences
                            for 0 till ith index, having count=j of first subsequence character, using at most k moves

we mark invalid states with Int.MinValue

then we recur by-

at ith index we can replace either by first character or second character or any other

the answer is max over j in dp(n - 1)(j)(m)
 */
object SubsequencesOfLengthTwo {
  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
    val arr = io.StdIn.readLine().toCharArray
    val Array(a, b) = io.StdIn.readLine().toCharArray

    val dp = Array.fill[Int](n, n + 1, m + 1)(Int.MinValue)


    // initialise, answer max for each (j, k) in dp(n-1)(j)(k)
    for {
      i <- 0 until n // position
      j <- 0 to n if j <= i + 1 // a's count
      k <- 0 to m // at most k moves
    } {
      dp(i)(j)(k) = {
        if (i == 0 && j == 0) 0
        else if (i == 0 && j == 1 && (arr(i) == a || k >= 1)) 0
        else {
          val aC = {
            if (i > 0 && j > 0 && arr(i) == a) dp(i - 1)(j - 1)(k) + (if (a == b) j - 1 else 0)
            else if (i > 0 && j > 0 && k > 0 && arr(i) != a) dp(i - 1)(j - 1)(k - 1) + (if (a == b) j - 1 else 0)
            else Int.MinValue
          }

          val bC = {
            if (i > 0 && arr(i) == b && j <= i) dp(i - 1)(j)(k) + j
            else if (i > 0 && k > 0 && arr(i) != b && j <= i) dp(i - 1)(j)(k - 1) + j
            else Int.MinValue
          }

          val nC = {
            if (i > 0 && arr(i) != a && arr(i) != b && j <= i) dp(i - 1)(j)(k) else Int.MinValue
          }

          if (aC < 0 && bC < 0 && nC < 0) Int.MinValue else aC max bC max nC
        }
      }
    }

    println {
      (0 to n).map(j => dp(n - 1)(j)(m)).max
    }

  }
}
