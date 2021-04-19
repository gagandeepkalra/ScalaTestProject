package google.kickstart._2021.B

/**
 * https://codingcompetitions.withgoogle.com/kickstart/round/0000000000435a5b/000000000077a882
 *
 * Given a string S of length N, you have to find out, for every position 1≤i≤N, what is the length of the longest strictly
 * increasing substring that ends at position i.
 */
object IncreasingSubstring {

  import scala.annotation.tailrec

  @tailrec
  def loop(input: List[Char], stackSize: Int, stackTop: Char, acc: List[Int]): List[Int] = {
    input match {
      case Nil                       => acc.reverse
      case c :: tail if stackTop < c => loop(tail, stackSize + 1, c, (stackSize + 1) :: acc)
      case c :: tail                 => loop(tail, 1, c, 1 :: acc)
    }
  }

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val _      = io.StdIn.readInt()
      val string = io.StdIn.readLine.toList

      val result = loop(string.tail, 1, string.head, 1 :: Nil)

      printFormattedOutput(t, result.mkString(" "))
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
