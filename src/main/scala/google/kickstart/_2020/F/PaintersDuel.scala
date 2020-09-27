package google.kickstart._2020.F

import algorithms.general.Memoize._

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff48/00000000003f47fb

[Game Theory, MiniMax Algorithm]

We play the game for all sequences, starting with A's move, having A's turn trying to maximize the score and B's turn minimizing it.
 */
object PaintersDuel {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(s, a1, a2, b1, b2, z) = io.StdIn.readLine.split(" ").map(_.toInt)

      val isBlocked: Array[Array[Boolean]] = {
        val arr = Array.ofDim[Boolean](s + 1, 2 * s)
        (1 to z).foreach { _ =>
          val temp = io.StdIn.readLine.split(" ").map(_.toInt)
          arr(temp(0))(temp(1)) = true
        }
        arr
      }

      val neighbours: ((Int, Int)) => List[(Int, Int)] = memoize {
        pair => {
          val (r, c) = pair
          var res: List[(Int, Int)] = Nil

          if (c - 1 > 0) res ::= (r, c - 1)

          if (c + 1 <= 2 * r - 1) res ::= (r, c + 1)

          if (r % 2 == 1) {
            if (c % 2 == 1) {
              if (r + 1 <= s) res ::= (r + 1, c + 1)
            } else {
              res ::= (r - 1, c - 1)
            }
          } else {
            if (c % 2 == 0) {
              res ::= (r - 1, c - 1)
            } else {
              if (r + 1 <= s) res ::= (r + 1, c + 1)
            }
          }

          res
        }
      }

      val visited: Array[Array[Boolean]] = Array.ofDim[Boolean](s + 1, 2 * s)
      visited(a1)(a2) = true
      visited(b1)(b2) = true

      def recur(x1: Int, y1: Int, score1: Int = 1, x2: Int, y2: Int, score2: Int = 1, turn: Boolean = true): Int = {
        val aCandidates = neighbours(x1, y1).filter { case (r, c) => !isBlocked(r)(c) && !visited(r)(c) }
        val bCandidates = neighbours(x2, y2).filter { case (r, c) => !isBlocked(r)(c) && !visited(r)(c) }

        if (aCandidates.isEmpty && bCandidates.isEmpty) score1 - score2
        else {
          if (turn) {
            // A's turn
            if (aCandidates.isEmpty) recur(x1, y1, score1, x2, y2, score2, !turn)
            else
              aCandidates.map { case (r, c) =>
                visited(r)(c) = true
                val ans = recur(r, c, score1 + 1, x2, y2, score2, !turn)
                visited(r)(c) = false
                ans
              }.max
          } else {
            // B's turn
            if (bCandidates.isEmpty) recur(x1, y1, score1, x2, y2, score2, !turn)
            else
              bCandidates.map { case (r, c) =>
                visited(r)(c) = true
                val ans = recur(x1, y1, score1, r, c, score2 + 1, !turn)
                visited(r)(c) = false
                ans
              }.min
          }
        }
      }

      printFormattedOutput(t, recur(a1, a2, 1, b1, b2, 1, turn = true))
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
