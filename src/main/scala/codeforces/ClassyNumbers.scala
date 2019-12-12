package codeforces

/*
https://codeforces.com/contest/1036/problem/C

Digit DP
 */
object ClassyNumbers {

  def main(args: Array[String]): Unit = {
    println {
      (for (_ <- 1 to io.StdIn.readInt()) yield {
        val Array(l, r) = io.StdIn.readLine().split(" ").map(_.toLong)

        val Classy = 4

        def solve(digits: Seq[Int]): Int = {

          val Idx = digits.size

          val dp = Array.fill[Option[Int]](Idx, Classy, 2)(None)

          def loop(idx: Int, flag: Boolean, classy: Int): Int = {

            if (classy == Classy)
              0
            else if (idx == Idx)
              1
            else
              dp(idx)(classy)(if (flag) 1 else 0).getOrElse {
                val res: Int = (1 to (if (flag) digits(idx) else 9))
                  .foldLeft(0)(
                    (acc, i) =>
                      acc + loop(idx + 1, flag && i == digits(idx), classy + 1)
                  ) + loop(idx + 1, flag && 0 == digits(idx), classy)

                dp(idx)(classy)(if (flag) 1 else 0) = Some(res)
                res
              }
          }

          loop(0, flag = true, 0)
        }

        val lDigits = (l - 1).toString.map(_ - '0')
        val rDigits = r.toString.map(_ - '0')

        solve(rDigits) - solve(lDigits)

      }).mkString("\n")
    }
  }

}
