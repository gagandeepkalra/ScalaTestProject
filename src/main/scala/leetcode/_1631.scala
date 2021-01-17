package leetcode

object _1631 {

  def minimumEffortPath(heights: Array[Array[Int]]): Int = {

    import scala.collection.mutable

    val n = heights.length
    val m = heights(0).length

    val deltas = List((1, 0), (-1, 0), (0, 1), (0, -1))

    def isValid(i: Int, max: Int): Boolean = 0 <= i && i < max

    def validNeighbours(x: Int, y: Int): List[(Int, Int)] = deltas.collect {
      case (dx, dy) if isValid(x + dx, n) && isValid(y + dy, m) => (x + dx, y + dy)
    }

    val visited = Array.ofDim[Boolean](n, m)

    val cost = Array.fill[Int](n, m)(Int.MaxValue)

    val queue = mutable.PriorityQueue.empty[(Int, Int, Int)](
      Ordering.by[(Int, Int, Int), Int](_._3).reverse
    )

    queue.enqueue((0, 0, 0))

    while (queue.nonEmpty) {
      val (x, y, givenCost) = queue.dequeue()
      if (!visited(x)(y)) {
        cost(x)(y) = givenCost
        visited(x)(y) = true
        validNeighbours(x, y).foreach {
          case (nX, nY) =>
            val nC = givenCost max (heights(nX)(nY) - heights(x)(y)).abs
            if (!visited(nX)(nY) && nC < cost(nX)(nY)) {
              cost(nX)(nY) = nC
              queue.enqueue((nX, nY, nC))
            }
        }
      }
    }

    cost.last.last
  }

  def main(args: Array[String]): Unit = {
    val input1 = Array(Array(1, 2, 2), Array(3, 8, 2), Array(5, 3, 5))
    val input2 = Array(Array(1, 2, 3), Array(3, 8, 4), Array(5, 3, 5))
    val input3 = Array(
      Array(1, 2, 1, 1, 1),
      Array(1, 2, 1, 2, 1),
      Array(1, 2, 1, 2, 1),
      Array(1, 2, 1, 2, 1),
      Array(1, 1, 1, 2, 1)
    )
    val input4 = Array(Array(1,10,6,7,9,10,4,9))
    println(minimumEffortPath(input1))
    println(minimumEffortPath(input2))
    println(minimumEffortPath(input3))
    println(minimumEffortPath(input4))
  }
}
