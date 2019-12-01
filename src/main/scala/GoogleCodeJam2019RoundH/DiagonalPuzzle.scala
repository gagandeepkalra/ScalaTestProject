package GoogleCodeJam2019RoundH

import scala.collection.mutable

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edd/00000000001a2835
# Bipartite Graph
 */
object DiagonalPuzzle {

  type Graph[T] = mutable.Map[T, (mutable.Set[T], mutable.Set[T])] // same and different

  def main(args: Array[String]): Unit = {
    println {
      (for (t <- 1 to io.StdIn.readInt()) yield {
        val n = io.StdIn.readInt()

        val graph: Graph[Int] =
          mutable
            .Map[Int, (mutable.Set[Int], mutable.Set[Int])]()
            .withDefaultValue((mutable.Set.empty[Int], mutable.Set.empty[Int]))

        val diagonals = 2 * n - 1

        for (i <- 0 until n) {
          val line: String = io.StdIn.readLine
          for (j <- 0 until n) {
            val leftDiagonal = n - 1 + j - i
            val rightDiagonal = i + j + diagonals // offset

            def update(node: Int, `with`: Int): Unit = {
              val (same, different) = graph.getOrElseUpdate(
                node,
                (mutable.Set.empty[Int], mutable.Set.empty[Int])
              )

              (if (line(j) == '#') same else different) += `with`
            }

            update(leftDiagonal, rightDiagonal)
            update(rightDiagonal, leftDiagonal)
          }
        }

        val visited: mutable.Set[Int] = mutable.Set.empty[Int]

        def dfs(node: Int, color: Int): (Int, Int) = { // color 0 and 1 count
          if (!visited(node)) {
            visited += node

            val (same, different) = graph(node)

            val (same_s, same_d) = same.foldLeft((0, 0)) {
              case ((acc_s, acc_d), neighbor) =>
                val (s, d) = dfs(neighbor, color)
                (acc_s + s, acc_d + d)
            }

            val (different_s, different_d) = different.foldLeft((0, 0)) {
              case ((acc_s, acc_d), neighbor) =>
                val (s, d) = dfs(neighbor, math.abs(color - 1))
                (acc_s + s, acc_d + d)
            }

            if (color == 0) (same_s + different_s + 1, same_d + different_d)
            else (same_s + different_s, same_d + different_d + 1)
          } else (0, 0)
        }

        val result = (for (i <- 0 until 2 * diagonals if !visited(i)) yield {
          val (s, d) = dfs(i, 0)
          s min d
        }).sum

        s"Case #$t: " + result
      }).mkString("\n")
    }
  }

}
