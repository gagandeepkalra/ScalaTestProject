import scala.collection.immutable.Queue

/**
  * Graph
  * Functional BFS
  */

/*
3 4
..#.
#...
..#.
1 2 3 4
2 3
3
2
5
10
 */
object BobAndCities {

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val board = Array.ofDim[Char](n, m)
    for (i <- 0 until n) {
      io.StdIn.readLine.toCharArray.zipWithIndex.foreach {
        case (c, j) => board(i)(j) = c
      }
    }

    val Array(l, r, u, d) = io.StdIn.readLine.split(" ").map(_.toLong) // cost
    val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt) // 1 based indexing

    def isValidIndex(i: Int, j: Int) = i >= 0 && i < n && j >= 0 && j < m && board(i)(j) != '#'

    val cost = new Array[Long](1000001)
    var k = 0


    breadthFirstSearch(Queue((x - 1, y - 1)), Map((x - 1, y - 1) -> 0))

    // Functional BFS

    def breadthFirstSearch(queue: Queue[(Int, Int)], map: Map[(Int, Int), Long]): Unit = {
      if (queue.isEmpty) println(-1)
      else {
        var ((x, y), q1) = queue.dequeue
        val parentCost = map((x, y))

        cost(k) = parentCost
        k += 1

        val dx = -1 :: 0 :: 1 :: 0 :: Nil
        val dy = 0 :: 1 :: 0 :: -1 :: Nil
        val c = u :: r :: d :: l :: Nil

        val (updatedQ, updatedMap) = dx.zip(dy).zip(c)
          .map(e => (x + e._1._1, y + e._1._2, e._2))
          .filter(e => isValidIndex(e._1, e._2))
          .foldLeft[(Queue[(Int, Int)], Map[(Int, Int), Long])]((q1, map)) {
          case ((queue: Queue[(Int, Int)], map: Map[(Int, Int), Long]), (x: Int, y: Int, c: Long)) =>

            if (map.contains((x, y))) {
              if (map((x, y)) > c + parentCost) {
                val updatedQ = queue.enqueue((x, y))
                val updatedMap = map + ((x, y) -> (c + parentCost))

                (updatedQ, updatedMap)
              } else {
                (queue, map)
              }
            } else {
              val updatedQ = queue.enqueue((x, y))
              val updatedMap = map + ((x, y) -> (c + parentCost))

              (updatedQ, updatedMap)
            }
        }

        breadthFirstSearch(updatedQ, updatedMap)
      }
    }

    System.out.print(-1)
  }

}
