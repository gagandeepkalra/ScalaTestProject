package google.kickstart._2020.H

object BoringNumbers {

  import scala.annotation.tailrec

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(l, r) = io.StdIn.readLine.split(" ").map(_.toLong)

      def recur(ls: List[Int], isEven: Boolean, ceil: Boolean): Long = {
        ls match {
          case Nil => 1
          case h :: tail if ceil =>
            val size =
              if (isEven) (0 until h by 2).size else (1 until h by 2).size

            val noCeil =
              if (size > 0) size * recur(tail, !isEven, ceil = false) else size

            val isHEven = h % 2 == 0
            noCeil + {
              if (isEven && isHEven || !isEven && !isHEven)
                recur(tail, !isEven, ceil = true)
              else 0
            }
          case _ :: tail =>
            5 * recur(tail, !isEven, ceil = false)

        }
      }

      def compute(x: Long): Long = {
        val ls = x.toString.map(_ - '0').toList

        @tailrec
        def countPrefixZeroes(list: List[Int], acc: Long): Long = {
          list match {
            case Nil => acc
            case _ :: tail =>
              countPrefixZeroes(tail, acc + recur(list, isEven = false, ceil = false))
          }
        }

        countPrefixZeroes(ls.tail, recur(ls, isEven = false, ceil = true))
      }

      printFormattedOutput(
        t,
        compute(r) - compute(l - 1)
      )
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
