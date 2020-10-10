package codeforces.contests._1426

/*
https://codeforces.com/contest/1426/problem/E

max is straightforward, we try win against each win combo

for min wins, we try
  a.) lose first all then draw or
  b.) draw first all then lose at each level

against each remaining opponent (we face each only once)

This way we try each combination and order to find the minimum result.
 */
object RockPaperScissors {

  def findMaxWins(r1: Int, s1: Int, p1: Int, r2: Int, s2: Int, p2: Int): Int =
    (r1 min s2) + (s1 min p2) + (p1 min r2)

  def findMinWins(r1: Int, s1: Int, p1: Int, r2: Int, s2: Int, p2: Int, turn: Int, remainingTurns: Set[Int]): Int = {

    if (remainingTurns.isEmpty) {
      val result = findMaxWins(r1, s1, p1, r2, s2, p2)
      result
    }
    else {
      turn match {
        case 1 => // rock
          remainingTurns.foldLeft(Int.MaxValue) { (acc, next) =>
            acc min {
              val core = r1 min (r2 + p2)
              val r11 = r1 - core

              {
                val r22 = r2 - (r2 min core)
                val p22 = p2 - (p2 min (core - (r2 min core)))
                findMinWins(r11, s1, p1, r22, s2, p22, next, remainingTurns - next)
              } min {
                val p22 = p2 - (p2 min core)
                val r22 = r2 - (r2 min (core - (p2 min core)))
                findMinWins(r11, s1, p1, r22, s2, p22, next, remainingTurns - next)
              }
            }
          }
        case 2 => // scissor
          remainingTurns.foldLeft(Int.MaxValue) { (acc, next) =>
            acc min {
              val core = s1 min (s2 + r2)
              val s11 = s1 - core

              {
                val s22 = s2 - (s2 min core)
                val r22 = r2 - (r2 min (core - (s2 min core)))
                findMinWins(r1, s11, p1, r22, s22, p2, next, remainingTurns - next)
              } min {
                val r22 = r2 - (r2 min core)
                val s22 = s2 - (s2 min (core - (r2 min core)))
                findMinWins(r1, s11, p1, r22, s22, p2, next, remainingTurns - next)
              }
            }
          }
        case 3 => // paper
          remainingTurns.foldLeft(Int.MaxValue) { (acc, next) =>
            acc min {
              val core = p1 min (p2 + s2)
              val p11 = p1 - core

              {
                val p22 = p2 - (p2 min core)
                val s22 = s2 - (s2 min (core - (p2 min core)))
                findMinWins(r1, s1, p11, r2, s22, p22, next, remainingTurns - next)
              } min {
                val s22 = s2 - (s2 min core)
                val p22 = p2 - (p2 min (core - (s2 min core)))
                findMinWins(r1, s1, p11, r2, s22, p22, next, remainingTurns - next)
              }
            }
          }
      }
    }
  }


  def main(args: Array[String]): Unit = {

    val n = io.StdIn.readInt()
    val Array(r1, s1, p1), Array(r2, s2, p2) = io.StdIn.readLine.split(" ").map(_.toInt)

    val maxWins = findMaxWins(r1, s1, p1, r2, s2, p2)
    val minWins = {
      val turns = Set(1, 2, 3)
      turns.map(t => findMinWins(r1, s1, p1, r2, s2, p2, t, turns - t)).min
    }

    println {
      s"$minWins $maxWins"
    }
  }
}
