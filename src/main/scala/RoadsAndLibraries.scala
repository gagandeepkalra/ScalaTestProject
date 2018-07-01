/**
  * https://www.hackerrank.com/challenges/torque-and-development/problem
 */
object RoadsAndLibraries {

  class Graph(size: Int) {

    class Node {
      var connectedNodes: List[Int] = Nil

      def connectTo(i: Int): Unit = connectedNodes = i :: connectedNodes
    }

    def connect(x: Int, y: Int): Unit = {
      nodes(x).connectTo(y)
      nodes(y).connectTo(x)
    }

    val nodes: Array[Node] = Array.fill[Node](size)(new Node)

    def countOfNodesInEachConnectedComponent: List[Int] = {
      val visited: Array[Boolean] = Array.fill[Boolean](size)(false)

      def dfs(i: Int): Int = {
        if (!visited(i)) {
          visited(i) = true
          nodes(i).connectedNodes.map(dfs).sum + 1
        } else 0
      }

      (1 until size).foldLeft[List[Int]](List())((result: List[Int], i: Int) => if (!visited(i)) dfs(i) :: result else result)
    }
  }

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt()).foreach(_ => {
      val Array(n, m, costLib, costRoad) = io.StdIn.readLine.split(" ").map(_.toLong)
      val graph: Graph = new Graph(n.toInt + 1)

      (1 to m.toInt).foreach(_ => {
        (graph.connect _).tupled {
          io.StdIn.readLine.split(" ").map(_.toInt) match {
            case Array(x, y) => (x, y)
          }
        }
      })

      System.out.println(math.min(costLib * n, graph.countOfNodesInEachConnectedComponent
        .map(_.toLong)
        .map(i => (i - 1) * costRoad + costLib).sum))
    })
  }
}
