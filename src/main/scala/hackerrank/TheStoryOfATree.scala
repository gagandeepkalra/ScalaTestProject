package hackerrank

/*
https://www.hackerrank.com/challenges/the-story-of-a-tree/problem

State transition for each node
 */
object TheStoryOfATree {

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b % a, a)

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val graph: Array[List[Int]] = {
        val arr = Array.fill[List[Int]](n + 1)(Nil)
        (1 until n).foreach { _ =>
          val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
          arr(x) = y :: arr(x)
          arr(y) = x :: arr(y)
        }
        arr
      }

      val Array(g, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val guesses: Array[Set[Int]] = {
        val empty = Set.empty[Int]
        val arr = Array.fill[Set[Int]](n + 1)(empty)
        (1 to g).foreach { _ =>
          val Array(u, v) = io.StdIn.readLine.split(" ").map(_.toInt)
          arr(u) = arr(u) + v
        }
        arr
      }

      def countCorrectGuessesFrom(node: Int, parent: Int): Int = {
        val children = graph(node).filterNot(_ == parent)
        val correctGuesses = children.count(guesses(node))

        children.foldLeft(correctGuesses)(_ + countCorrectGuessesFrom(_, node))
      }

      def countRootsWithMoreThanKCorrectGuesses(node: Int,
                                                parent: Int,
                                                parentScore: Int): Int = {

        val nodeScore: Int =
          if (guesses(node)(parent) && !guesses(parent)(node))
            parentScore + 1
          else if (!guesses(node)(parent) && guesses(parent)(node))
            parentScore - 1
          else
            parentScore

        graph(node).foldLeft(if (nodeScore >= k) 1 else 0)(
          (acc, child) =>
            if (child == parent)
              acc
            else
              acc + countRootsWithMoreThanKCorrectGuesses(
                child,
                node,
                nodeScore
            )
        )

      }

      val rootScore = countCorrectGuessesFrom(1, -1)
      val wins = graph(1).foldLeft(if (rootScore >= k) 1 else 0)(
        _ + countRootsWithMoreThanKCorrectGuesses(_, 1, rootScore)
      )

      println {
        val gcd: Int = TheStoryOfATree.gcd(wins, n)
        s"${wins / gcd}/${n / gcd}"
      }
    }
  }

}
