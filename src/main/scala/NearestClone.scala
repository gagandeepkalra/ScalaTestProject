import scala.collection.immutable.Queue
/*
https://www.hackerrank.com/challenges/find-the-nearest-clone/problem
 */
object NearestClone {

  class Graph[T](size: Int) {

    class Node {
      var connectedNodes: List[Int] = Nil
      var color: Int = 0

      def connectTo(i: Int): Unit = connectedNodes = i :: connectedNodes
    }

    def connect(x: Int, y: Int): Unit = {
      nodes(x).connectTo(y)
      nodes(y).connectTo(x)
    }

    def connectOneWay(x: Int, y: Int): Unit = {
      nodes(x).connectTo(y)
    }

    val nodes: Array[Node] = Array.fill[Node](size)(new Node)
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val Array(n, m) = stdin.readLine().split(" ").map(_.toInt)

    val graph = new Graph(n + 1)
    (1 to m).foreach { _ =>
      val Array(x, y) = stdin.readLine.split(" ").map(_.toInt)
      graph.connect(x, y)
    }

    var idx = 1
    stdin.readLine.split(" ").foreach { i =>
      val color = i.toInt
      graph.nodes(idx).color = color
      idx += 1
    }
    val query = io.StdIn.readInt

    val colorGroups: Map[Int, Array[graph.Node]] = graph.nodes.groupBy(_.color)
    val colorIndices: Map[Int, Int] = colorGroups.keySet.zipWithIndex.map { case (color, index) => (color, index + 1) }.toMap

    if (!colorIndices.contains(query)) {
      System.out.println(-1)
      return
    }

    val queryIndex = colorIndices(query)

    val newGraph = new Graph(colorGroups.size + 1)
    colorGroups.foreach { case (color, nodes) =>
      for {
        node <- nodes
        neighbour <- node.connectedNodes
      } newGraph.connectOneWay(colorIndices(color), colorIndices(graph.nodes(neighbour).color))

      newGraph.nodes(colorIndices(color)).color = color
    }


    def breadthFirstSearch(queue: Queue[Int], parentMap: Map[Int, Int], visited: Set[Int]): Map[Int, Int] = {
      if (queue.nonEmpty) {
        val (elem, newQueue) = queue.dequeue
        val newVisited = visited + elem

        val par = parentMap.getOrElse(elem, -1)
        val neighboursWithSize = newGraph.nodes(elem).connectedNodes.groupBy(identity).map { case (x, ls) => (x, ls.size) }

        if (par == queryIndex && neighboursWithSize(queryIndex) > 1 || par != queryIndex && neighboursWithSize.contains(queryIndex)) {
          parentMap + (queryIndex -> elem)
        } else {
          val (q, m) = neighboursWithSize
            .filter { case (i, count) => !visited(i) }
            .foldLeft((newQueue, parentMap)) { case ((que, map), (i, count)) =>
              (que.enqueue(i), map + (i -> elem))
            }
          breadthFirstSearch(q, m, newVisited)
        }
      } else parentMap
    }

    val parent = breadthFirstSearch(Queue(queryIndex), Map(queryIndex -> -1), Set())

    if (parent.getOrElse(queryIndex, -1) != -1) {
      var result = 1
      var i = parent(queryIndex)
      while (i != queryIndex) {
        result += 1
        i = parent(i)
      }
      System.out.println(result)

    } else System.out.println(-1)

  }
}
