package leetcode

/**
  * https://leetcode.com/problems/guess-number-higher-or-lower-ii
  *
  * We are playing the Guessing Game. The game will work as follows:
  *
  * I pick a number between 1 and n.
  * You guess a number.
  * If you guess the right number, you win the game.
  * If you guess the wrong number, then I will tell you whether the number I picked is higher or lower, and you will continue guessing.
  * Every time you guess a wrong number x, you will pay x dollars. If you run out of money, you lose the game.
  *
  * Given a particular n, return the minimum amount of money you need to guarantee a win regardless of what number I pick.
  */
object _375 {

  def getMoneyAmount(n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1, n + 1)

    for {
      j <- 1 to n
      i <- j - 1 to 1 by -1
      _ = dp(i)(j) = Int.MaxValue
      k <- i to j
    } {
      dp(i)(j) = dp(i)(j) min {
        k + {
          (if (k - 1 >= i) dp(i)(k - 1) else 0) max
            (if ((k + 1) <= j) dp(k + 1)(j) else 0)
        }
      }
    }

    dp(1)(n)
  }

  def main(args: Array[String]): Unit = {
    println(getMoneyAmount(10))
  }
}
