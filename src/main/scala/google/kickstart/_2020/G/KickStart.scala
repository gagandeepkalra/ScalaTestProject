package google.kickstart._2020.G

/*
https://codingcompetitions.withgoogle.com/kickstart/round/00000000001a0069/0000000000414bfb

find all indices for KICK and START, for KICK make sure to include the second K

afterwards match each KICK to all possible START and add to result
 */
object KickStart {

  val from = "KICK"
  val to = "START"

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val s = io.StdIn.readLine()

      @scala.annotation.tailrec
      def findIndices(input: String, idx: Int = 0, acc: List[Int] = Nil): List[Int] = {
        if (idx >= s.length) acc
        else {
          val next = s.indexOf(input, idx)
          if (next == -1) acc
          else findIndices(input, next + input.length - 1, next :: acc)
        }
      }

      val fromIndices = findIndices(from).reverse
      val toIndices = findIndices(to).zipWithIndex.reverse

      @scala.annotation.tailrec
      def count(fromIndices: List[Int], toIndices: List[(Int, Int)], acc: Int = 0): Int = {
        if (fromIndices.isEmpty || toIndices.isEmpty) acc
        else {
          val f :: fTail = fromIndices

          val tRest = toIndices.dropWhile(_._1 < f)

          count(fTail, tRest, acc + tRest.headOption.fold(0)(_._2 + 1))
        }
      }

      val result = count(fromIndices, toIndices)

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
