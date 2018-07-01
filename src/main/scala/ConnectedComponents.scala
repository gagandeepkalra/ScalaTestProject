object ConnectedComponents {

  class Graph(size: Int) {

    class Node {
      var connectedNodes: List[Int] = Nil

      def connectTo(i: Int): Unit = connectedNodes = i :: connectedNodes
    }

    def connect(x: Int, y: Int): Unit = {
      nodes(x).connectTo(y)
      nodes(y).connectTo(x)
    }

    def connectedComponentsCost(): Int = {
      val visited = Array.fill[Boolean](size)(false)

      def connectedComponentsSizeStartingFrom(i: Int): Int = {
        if (visited(i)) 0
        else {
          visited(i) = true
          nodes(i).connectedNodes.map(connectedComponentsSizeStartingFrom).sum + 1
        }
      }

      (1 until size).filter(!visited(_)).map(connectedComponentsSizeStartingFrom).map(math.sqrt(_).ceil.toInt).sum
    }

    val nodes: Array[Node] = Array.fill[Node](size)(new Node)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val m = io.StdIn.readInt()

    val graph: Graph = new Graph(n + 1)

    (1 to m).foreach(_ => {
      (graph.connect _).tupled(io.StdIn.readLine.split(" ").map(_.toInt) match {
        case Array(x, y) => (x, y)
        case _ => (0, 0)
      })
    })

    println(graph.connectedComponentsCost())
  }
}
