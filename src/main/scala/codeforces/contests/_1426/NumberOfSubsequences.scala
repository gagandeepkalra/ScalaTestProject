package codeforces.contests._1426

/*
https://codeforces.com/contest/1426/problem/F

[Dynamic Programming]

we need count the total number of subsequences

we use dp to save number of subsequences for each i, abc, bc, c, afterwards dp(0)(abc) is the solution

if `a` we extend from previous result by adding bc
if `b` we extend from previous result by adding c
if `c` we extend from previous result by adding #subsequences

if `?` we extend each of abc, bc, c by tripling them first, afterwards adding respectively like above

 */
object NumberOfSubsequences {
  val MOD: Long = 1000000007

  def main(args: Array[String]): Unit = {

    val n = io.StdIn.readInt
    val s = io.StdIn.readLine

    var count: Long = 1

    val dp: Array[Array[Long]] = Array.ofDim[Long](n, 3)

    s(n - 1) match {
      case 'c' =>
        dp(n - 1)(2) = 1
      case '?' =>
        dp(n - 1)(2) = 1
        count = 3
      case _ =>
    }

    for (i <- n - 2 to 0 by -1) {

      dp(i)(0) = dp(i + 1)(0)
      dp(i)(1) = dp(i + 1)(1)
      dp(i)(2) = dp(i + 1)(2)

      s(i) match {
        case 'a' => dp(i)(0) = (dp(i)(0) + dp(i + 1)(1)) % MOD
        case 'b' => dp(i)(1) = (dp(i)(1) + dp(i + 1)(2)) % MOD
        case 'c' => dp(i)(2) = (dp(i)(2) + count) % MOD
        case '?' =>
          dp(i)(0) = (dp(i)(0) * 3) % MOD
          dp(i)(1) = (dp(i)(1) * 3) % MOD
          dp(i)(2) = (dp(i)(2) * 3) % MOD

          dp(i)(0) = (dp(i)(0) + dp(i + 1)(1)) % MOD
          dp(i)(1) = (dp(i)(1) + dp(i + 1)(2)) % MOD
          dp(i)(2) = (dp(i)(2) + count) % MOD

          count = (count * 3) % MOD
      }

    }

    println(dp(0)(0))
  }
}
