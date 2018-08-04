object Pramata {

  /*
 There were n submissions made in a programming contest containing infinite problems. Each submission earned the contestant 1
 point as none of the submissions is a wrong or a partial submission. You are given the details of the submissions -
 the username of the contestant and the time taken to solve the problem.
 Your task is to print the rank list according to the following rules:

 The contestant with a higher score gets a higher rank.
 If the scores are tied, then the contestant with the least sum of the time taken to solve the problems gets a higher rank.
 In case of a tie in both scores and sum of the time taken, they are ranked lexicographically according to their usernames.
 Note: The details of the submissions are not sorted in any order (neither by time nor by username)
  */
  def groupingAndSorting(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt)
      .map(_ => io.StdIn.readLine().split(" "))
      .groupBy(_ (0))
      .map { case (str, arr) => (str, arr.size, arr.map(_ (1).toInt).sum) }
      .toArray
      .sortWith { case ((n1, s1, t1), (n2, s2, t2)) => s1 > s2 || s1 == s2 && (t1 < t2 || t1 == t2 && n1 < n2) }
      .zipWithIndex
      .foreach { case ((name, _, _), rank) => System.out.println(rank + 1 + " " + name) }
  }

  /*
  You are given a stack of N integers. In one operation, you can either pop an element from the stack or push any popped
  element into the stack. You need to maximize the top element of the stack after performing exactly K operations. If
  the stack becomes empty after performing K operations and there is no other way for the stack to be non-empty, print -1.
   */

  def maxFromAStackWithFixedNumberOfOperations(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
    val arr: Array[Long] = io.StdIn.readLine.split(" ").map(_.toLong)

    System.out.println(
      if (n == 1) if (k % 2 == 1) -1 else arr(0)
      else if (k == n) getMaxFromArray(arr, k - 1)
      else if (k > n) arr.max
      else math.max(getMaxFromArray(arr, k - 1), arr(k))
    )

    def getMaxFromArray(arr: Array[Long], k: Int): Long = {
      var max = -1l
      for (i <- 0 until k) max = math.max(max, arr(i))
      max
    }

  }

  /*
  link- https://www.hackerearth.com/challenge/hiring/pramata-software-engineer-hiring-challenge/algorithm/treasure-island-3ecc4b6f/
  Summary- find minimum cost to cut a matrix into squares of size 1x1, cost = sum of each element of the matrix
   */
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

    println(minCost(0, 0, row - 1, col - 1))

    def minCost(x1: Int, y1: Int, x2: Int, y2: Int): Long = {

      if (x1 == x2 && y1 == y2) 0
      else if (x2 - x1 == 1 && y2 - y1 == 1 || x1 == x2 && y1 + 1 == y2 || x1 + 1 == x2 && y1 == y2) sum(x1, y1, x2, y2)
      else {
        var result = Long.MaxValue

        for (i <- x1 + 1 until x2) {
          result = math.min(result, minCost(x1, y1, i - 1, y2) + minCost(i, y1, x2, y2))
        }

        for (j <- y1 + 1 until y2) {
          result = math.min(result, minCost(x1, y1, x2, j - 1) + minCost(x1, j, x2, y2))
        }

        result + sum(x1, y1, x2, y2)
      }
    }

    def sum(x1: Int, y1: Int, x2: Int, y2: Int): Long = {

      var sum = sumArr(x2)(y2)
      if (x1 != 0) sum -= sumArr(x1 - 1)(y2)
      if (y1 != 0) sum -= sumArr(x2)(y1 - 1)
      if (x1 != 0 && y1 != 0) sum += sumArr(x1 - 1)(y1 - 1)

      sum
    }
  }
}
