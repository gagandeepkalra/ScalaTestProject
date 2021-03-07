package codeforces.contests._1487

/*
http://codeforces.com/contest/1487/problem/C

decompose to find draw condition afterwards distribute wins equally amongst all
 */
object MinimumTies {

  def main(args: Array[String]): Unit = {
    println({
      for (_ <- 1 to io.StdIn.readInt()) yield {
        val n = io.StdIn.readInt()

        val EachTeamWins = (n - 1) / 2

        val teamWins = Array.fill(n + 1)(0)

        def drawCheck(i: Int, j: Int): Boolean = n % 2 == 0 && (j - i) == n / 2

        {
          for {
            i <- 1 to n
            j <- i + 1 to n
          } yield {
            if (drawCheck(i, j)) 0
            else if (teamWins(i) < EachTeamWins) {
              teamWins(i) += 1
              1
            } else if (teamWins(j) < EachTeamWins) {
              teamWins(j) += 1
              -1
            } else {
              0
            }
          }
        }.mkString(" ")
      }
    }.mkString("\n"))
  }
}
