package codeforces

/*
https://codeforces.com/gym/100886/problem/G

Digit DP
 */
object MaximumProduct {

  import scala.language.implicitConversions

  def main(args: Array[String]): Unit = {
    val Array(l, r) = io.StdIn.readLine().split(" ").map(_.toLong)

    val rDigits = r.toString.map(_ - '0')
    val Idx = rDigits.length

    val lDigits = {
      val digits = l.toString.map(_ - '0')
      List.fill(Idx - digits.length)(0) ++ digits
    }

    val dp = Array.fill[Option[(Long, List[Int])]](Idx, 2, 2)(None) // (idx, upperFlag, lowerFlag), 1 (true) => limit, 0 (false) => no limit

    def loop(idx: Int,
             upperFlag: Boolean,
             lowerFlag: Boolean): (Long, List[Int]) = {

      if (idx == Idx)
        (1L, Nil)
      else
        dp(idx)(upperFlag)(lowerFlag).getOrElse {
          val lower = if (lowerFlag) lDigits(idx) else 0
          val upper = if (upperFlag) rDigits(idx) else 9
          val tuple = (lower to upper)
            .foldLeft((Long.MinValue, List.empty[Int])) {
              case (acc @ (product, _), i) =>
                val (nextProduct, nextList) = loop(
                  idx + 1,
                  upperFlag && i == rDigits(idx),
                  lowerFlag && i == lDigits(idx)
                )
                if (i * nextProduct > product) (i * nextProduct, i :: nextList)
                else acc
            }

          dp(idx)(upperFlag)(lowerFlag) = Some(tuple)
          tuple
        }
    }

    println {
      loop(0, upperFlag = true, lowerFlag = true)._2
        .dropWhile(_ == 0)
        .mkString("")
    }

  }

  implicit def toInt(b: Boolean): Int = if (b) 1 else 0

}
