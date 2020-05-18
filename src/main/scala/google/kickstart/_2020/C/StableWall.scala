package google.kickstart._2020.C

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff43/00000000003379bb

[Topological Sort]

Check if graph is acyclic then topological sort
 */
object StableWall {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(r, c) = io.StdIn.readLine.split(" ").map(_.toInt)
      val a = (0 until r).map(_ => io.StdIn.readLine.map(_ - 'A'))

      val graph = collection.mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set.empty[Int])

      val allChars = (for (i <- 0 until r; j <- 0 until c) yield a(i)(j)).toSet

      for (i <- 0 until r - 1; j <- 0 until c) if (a(i)(j) != a(i + 1)(j))
        graph.update(a(i)(j), graph(a(i)(j)) + a(i + 1)(j))

      val isAcyclic = {
        val visited = new Array[Boolean](26)

        def acyclic(i: Int, parents: Set[Int] = Set.empty): Boolean = {
          visited(i) || !parents(i) && {
            val newParents = parents + i
            val result = graph(i) forall (acyclic(_, newParents))
            visited(i) = true
            result
          }
        }

        allChars.forall(acyclic(_))
      }

      def order: String = {
        val visited = new Array[Boolean](26)

        var stack = List.empty[Int]

        def topologicalSort(i: Int): Unit =
          if (!visited(i)) {
            graph(i).foreach(topologicalSort)
            visited(i) = true
            stack ::= i
          }

        allChars.foreach(topologicalSort)

        stack.reverse.map(i => (i + 'A').toChar).mkString("")
      }

      printFormattedOutput(t, if (isAcyclic) order else -1)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
