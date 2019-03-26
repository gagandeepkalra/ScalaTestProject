package codeforces.contest1139

object EdgyTrees {
  type Graph = Map[Int, Set[Int]]

  def addBidirectionalEdge(graph: Graph, x: Int, y: Int, color: Boolean): Graph = {
    val setX: Set[Int] = graph.getOrElse(x, Set())
    val setY: Set[Int] = graph.getOrElse(y, Set())

    graph + (x -> (setX + y)) + (y -> (setY + x))
  }

  def addSingleVertices(graph: Graph, v: List[Int]): Graph = {
    v.foldLeft(graph)((g, v) => g + (v -> g.getOrElse(v, Set())))
  }


  def connectedVertexSet(root: Int, graph: Graph): Set[Int] = {
    def connectedVertexSet(v: Int, p: Int)(implicit set: collection.mutable.Set[Int]): Unit= {
      for {
        neighbors <- graph.get(v)
        neighbor <- neighbors
        if neighbor != p
      } {
        set += neighbor
        connectedVertexSet(neighbor, v)
      }
    }

    val result = collection.mutable.Set[Int](root)
    connectedVertexSet(root, -1)(result)
    result.toSet
  }

  def connectedComponentSizesList(graph: Graph): List[Int] = {
    val visited = collection.mutable.Set[Int]()
    graph.keySet.foldLeft(List[Int]()) {
      case (result: List[Int], root: Int) =>
        if (!visited(root)) {
          val vertexSet = connectedVertexSet(root, graph)
          visited ++= vertexSet
          vertexSet.size :: result
        } else result
    }
  }


  def power(n: Int, k: Int, mod: Int): Int = {
    (1 to k).foldLeft(1l)((res, _) => (res * n) % mod).toInt
  }

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine().split(" ").map(_.toInt)

    val graph = (1 until n).foldLeft(Map[Int, Set[Int]]()) { case (graph: Graph, _) =>
      val Array(x, y, color) = io.StdIn.readLine().split(" ").map(_.toInt)
      val g = addSingleVertices(graph, List(x, y))
      if (color == 0)
        addBidirectionalEdge(g, x, y, color == 1)
      else
        g
    }

    val connectedComponentsSizes = connectedComponentSizesList(graph)

    val mod = 1000000007

    val result = power(n, k, mod) - connectedComponentsSizes.foldLeft(0l)((res, size) => (res + power(size, k, mod)) % mod) // n^k - sigma p^k

    println(if (result < 0) result + mod else result)
  }
}
