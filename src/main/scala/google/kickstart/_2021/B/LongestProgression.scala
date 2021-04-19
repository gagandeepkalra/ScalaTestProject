package google.kickstart._2021.B

/**
  * https://codingcompetitions.withgoogle.com/kickstart/round/0000000000435a5b/000000000077a3a5
  *
  * An arithmetic array is an array that contains at least two integers and the differences between consecutive integers
  * are equal. For example, [9,10], [3,3,3], and [9,7,5,3] are arithmetic arrays, while [1,3,3,7], [2,1,2], and [1,2,4]
  * are not.
  *
  * For an array A, Sarasvati defines a subarray as any contiguous part of A. Please help Sarasvati determine the length
  * of the longest possible arithmetic subarray she can create by replacing at most one element in the original array.
  *
  * We calculate left view and right view, then consider each element as candidate for replacement one by one, answer is
  * the max of all possibilities.
  *
  * left view: array of tuple (#elements, delta?) wherein adding delta to current element gives the element left to it.
  * right view: array of tuple (#elements, delta?) wherein adding delta to current element gives the element right to it.
  *
  * where delta is optional for the first and last element have nothing to the left and right of them respectively.
  */
object LongestProgression {
  import scala.annotation.tailrec

  @tailrec
  def rightView(
    input: List[Long],
    stackTop: Long,
    stackSize: Int,
    stackDelta: Long,
    acc: List[(Int, Option[Long])]
  ): List[(Int, Option[Long])] = {
    input match {
      case Nil =>
        acc
      case c :: tail if c + stackDelta == stackTop =>
        rightView(tail, c, stackSize + 1, stackDelta, (stackSize + 1, Some(stackDelta)) :: acc)
      case c :: tail =>
        rightView(tail, c, 2, stackTop - c, (2, Some(stackTop - c)) :: acc)
    }
  }

  @tailrec
  def leftView(
    input: List[Long],
    stackTop: Long,
    stackSize: Int,
    stackDelta: Long,
    acc: List[(Int, Option[Long])]
  ): List[(Int, Option[Long])] = {
    input match {
      case Nil =>
        acc.reverse
      case c :: tail if stackTop + stackDelta == c =>
        leftView(tail, c, stackSize + 1, stackDelta, (stackSize + 1, Some(-stackDelta)) :: acc)
      case c :: tail =>
        leftView(tail, c, 2, c - stackTop, (2, Some(stackTop - c)) :: acc)
    }
  }

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n     = io.StdIn.readInt()
      val input = io.StdIn.readLine.split(" ").map(_.toLong).toList

      val rightViewArray: Array[(Int, Option[Long])] = {
        val inputR = input.reverse
        val delta  = inputR.head - inputR.tail.head
        rightView(inputR.tail.tail, inputR.tail.head, 2, delta, (2, Some(delta)) :: (1, None) :: Nil).toArray
      }

      val leftViewArray: Array[(Int, Option[Long])] = {
        val delta = input.tail.head - input.head
        leftView(input.tail.tail, input.tail.head, 2, delta, (2, Some(-delta)) :: (1, None) :: Nil).toArray
      }

      val inputA = input.toArray

      val result = (0 until n).map {
        case 0                 => 1 + rightViewArray(1)._1
        case i if i == (n - 1) => leftViewArray(n - 2)._1 + 1
        case i =>
          val l            = inputA(i - 1)
          val (lC, lDelta) = leftViewArray(i - 1)
          val r            = inputA(i + 1)
          val (rC, rDelta) = rightViewArray(i + 1)

          val result = (lDelta, rDelta) match {
            case (Some(lD), Some(rD)) if -lD == rD && l + 2 * rD == r => lC + rC + 1
            case (_, Some(rD)) if l + 2 * rD == r                     => lC max (rC + 2)
            case (Some(lD), _) if r + 2 * lD == l                     => (lC + 2) max rC
            case _                                                    => (lC + 1) max (rC + 1)
          }

          result
      }.max

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
