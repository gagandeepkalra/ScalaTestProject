object PramataSoftware {


// Q- find minimum cost to cut a matrix into squares of size 1x1, cost = sum of each element of the matrix
  def main(args: Array[String]): Unit = {
    val Array(row, col) = io.StdIn.readLine.split(" ").map(_.toInt)

    var board = Array.ofDim[Int](row, col)
    for (i <- 0 until row) {
      io.StdIn.readLine.split(" ").map(_.toInt).zipWithIndex.foreach {
        case (value, j) => board(i)(j) = value
      }
    }

    var sumArr = Array.ofDim[Long](row, col)

    for (i <- 0 until row;
         j <- 0 until col) {
      if (i == 0 && j != 0) sumArr(i)(j) = sumArr(i)(j - 1)
      else if (i != 0 && j == 0) sumArr(i)(j) = sumArr(i - 1)(j)
      else if (i != 0 && j != 0) sumArr(i)(j) = sumArr(i - 1)(j) + sumArr(i)(j - 1) - sumArr(i - 1)(j - 1)

      sumArr(i)(j) += board(i)(j)
    }


    def minCost(x1: Int, y1: Int, x2: Int, y2: Int): Long = {

      if (x1 == x2 && y1 == y2) {
        return 0
      }

      if(x2-x1==1 && y2-y1==1) return sum(x1, y1, x2, y2)

      if (x1 == x2 && y1 + 1 == y2 || x1 + 1 == x2 && y1 == y2) {
        return sum(x1, y1, x2, y2)
      }

      var result = Long.MaxValue

      for (i <- x1 + 1 until x2) {
        result = math.min(result, minCost(x1, y1, i - 1, y2) + minCost(i, y1, x2, y2))
      }

      for (j <- y1 + 1 until y2) {
        result = math.min(result, minCost(x1, y1, x2, j - 1) + minCost(x1, j, x2, y2))
      }

      result + sum(x1, y1, x2, y2)
    }

    def sum(x1: Int, y1: Int, x2: Int, y2: Int): Long = {

      var sum = sumArr(x2)(y2)
      if (x1 != 0) sum -= sumArr(x1 - 1)(y2)
      if (y1 != 0) sum -= sumArr(x2)(y1 - 1)
      if (x1 != 0 && y1 != 0) sum += sumArr(x1 - 1)(y1 - 1)

      sum
    }
    println(minCost(0, 0, row - 1, col - 1))
  }
}
