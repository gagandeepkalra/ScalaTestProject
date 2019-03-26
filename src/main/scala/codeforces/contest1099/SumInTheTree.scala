package codeforces.contest1099

object SumInTheTree {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    val parentOf: Array[Int] = Array(0, 0) ++ io.StdIn.readLine.split(" ").map(_.toInt)

    val childrenOf: Map[Int, Set[Int]] = (2 to n).foldLeft(Map[Int, Set[Int]]().withDefaultValue(Set[Int]()))((map, i) => map + (parentOf(i) -> (map(parentOf(i)) + i)))

    val rootToNodeSumOf: Array[Long] = 0l +: io.StdIn.readLine.split(" ").map(_.toLong)

    def getTotalSum(i: Int): Long = {
      if (rootToNodeSumOf(i) == -1) {
        rootToNodeSumOf(i) = if (childrenOf(i).nonEmpty) childrenOf(i).filter(_ != parentOf(i)).map(rootToNodeSumOf).foldLeft(Long.MaxValue)(_ min _)
        else rootToNodeSumOf(parentOf(i))

        if (rootToNodeSumOf(i) < rootToNodeSumOf(parentOf(i))) {
          println(-1)
          System.exit(0)
        }
      }

      childrenOf(i).filter(_ != parentOf(i)).map(child => getTotalSum(child)).foldLeft(0l)(_ + _) + rootToNodeSumOf(i) - rootToNodeSumOf(parentOf(i))
    }

    println(getTotalSum(1))
  }

}
