package codeforces.contests._1462

object AddToNeighbourAndRemove {
  import scala.annotation.tailrec

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()
      val a = io.StdIn.readLine.split(" ").map(_.toInt)

      val sum = a.sum

      @tailrec
      def areAllGroupsSame(target: Int, i: Int = 0, acc: Int = 0): Boolean = {
        if (acc > target) false
        else if (i == n) acc == 0
        else { // acc <= target
          val accU = acc + a(i)

          if (accU < target) areAllGroupsSame(target, i + 1, accU)
          else if (accU == target) areAllGroupsSame(target, i + 1)
          else false
        }

      }

      val sqrt = math.sqrt(sum).ceil.toInt

      @tailrec
      def findAllDivisors(i: Int = 1, acc: List[Int] = Nil): List[Int] = {
        if (i > sqrt) acc.distinct.sorted
        else {
          if (sum % i == 0) findAllDivisors(i + 1, i :: sum / i :: acc)
          else findAllDivisors(i + 1, acc)
        }
      }

      val result = findAllDivisors()
        .filter(areAllGroupsSame(_))
        .map(divisor => n - (sum / divisor))
        .min

      println(result)
    }
  }
}
