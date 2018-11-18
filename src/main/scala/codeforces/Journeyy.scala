package codeforces

/*

http://codeforces.com/problemset/problem/839/C
DFS
 */
object Journeyy {

  type Graph = Map[Int, Set[Int]]

  def update(graph: Graph, x: Int, y: Int): Graph = {
    val setX: Set[Int] = graph.withDefaultValue(Set())(x)
    val setY: Set[Int] = graph.withDefaultValue(Set())(y)
    graph + (x -> (setX + y)) + (y -> (setY + x))
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    val graph: Graph = (1 until n).foldLeft[Graph](Map[Int, Set[Int]](1 -> Set(0)))((acc, _) => {
      val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
      update(acc, x, y)
    })

    println(calculateExpectedValue(1, 0, 0, 1.0))

    def calculateExpectedValue(me: Int, parent: Int, distance: Int, probability: Double): Double = {
      if (graph(me).size == 1) {
        probability * distance
      } else {
        val count = graph(me).size - 1
        val updatedProbability = probability * (1.0/count)

        graph(me).filter(_ != parent)
          .foldLeft[Double](0.0)((acc, child) => acc + calculateExpectedValue(child, parent = me, distance + 1, updatedProbability))
      }
    }
  }

}
