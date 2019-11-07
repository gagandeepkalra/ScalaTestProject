package codeforces.contest1244

object PaintTheTree {

  type Graph = Map[Int, Set[Int]]

  def update(graph: Graph, x: Int, y: Int): Graph = {
    val setX: Set[Int] = graph.withDefaultValue(Set.empty)(x)
    val setY: Set[Int] = graph.withDefaultValue(Set.empty)(y)
    graph + (x -> (setX + y)) + (y -> (setY + x))
  }

  def linearize(graph: Graph): List[Int] = {

    @scala.annotation.tailrec
    def loop(node: Int, acc: List[Int]): List[Int] = {

      acc match {
        case Nil => loop(graph(node).head, node :: acc)
        case h :: _ =>
          graph(node).find(_ != h) match {
            case Some(next) => loop(next, node :: acc)
            case None       => node :: acc
          }
      }
    }

    loop(graph.find(_._2.size == 1).get._1, Nil)
  }

  def expandListToSize(n: Int, ls: List[Int]): List[Int] = {
    val size = ls.length
    (1 to n / size + 1).toList.flatMap(_ => ls).take(n)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val colorA, colorB, colorC = io.StdIn.readLine.split(" ").map(_.toInt)

    val getColor = Seq(colorA, colorB, colorC)

    val graph: Graph = (1 until n).foldLeft(Map[Int, Set[Int]]()) { (acc, _) =>
      val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
      update(acc, x, y)
    }

    def colorToValue(color: Int, node: Int): Long = getColor(color - 1)(node - 1).toLong

    println {
      if (graph.values.forall(_.size <= 2)) {

        val seq = linearize(graph)

        val solutionList = List(
          expandListToSize(n, List(1, 2, 3)),
          expandListToSize(n, List(1, 3, 2)),
          expandListToSize(n, List(2, 1, 3)),
          expandListToSize(n, List(2, 3, 1)),
          expandListToSize(n, List(3, 2, 1)),
          expandListToSize(n, List(3, 1, 2))
        )

        val (min, list) = solutionList
          .map { ls =>
            val zipped = ls.zip(seq)
            (zipped.map((colorToValue _).tupled).sum, zipped.sortBy(_._2).map(_._1))
          }
          .minBy(_._1)

        min + "\n" + list.mkString(" ")
      } else -1
    }

  }
}
