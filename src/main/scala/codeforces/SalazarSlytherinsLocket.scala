package codeforces

/*
https://codeforces.com/contest/855/problem/E

Digit DP
 */
object SalazarSlytherinsLocket {

  val dp: Array[Array[Array[Array[Long]]]] =
    Array.fill(11, 2, 1024, 60)(-1)

  def solve(value: Long, radix: Int): Long = {
    val seq: Seq[Int] = {
      BigInt(value).toString(radix).map(_ - '0')
    }
    val seqL = seq.length

    def recur(flag: Boolean = false, zeroes: Boolean, mask: Int, index: Int): Long = {
      if (index < 0) if (mask == 0) 1 else 0
      else {
        val saved = dp(radix)(if (zeroes) 1 else 0)(mask)(index)
        if (!flag && saved != -1) saved
        else {
          val upper = if (flag) seq(seqL - 1 - index) else radix - 1
          val result = (0 to upper).foldLeft(0L)((acc, i) => {
            acc + (
              if (i == 0 && zeroes)
                recur(flag && i == upper, zeroes = true, mask, index - 1)
              else
                recur(flag && i == upper, zeroes = false, mask ^ (1 << i), index - 1)
              )
          })

          if (!flag) dp(radix)(if (zeroes) 1 else 0)(mask)(index) = result
          result
        }
      }
    }

    recur(flag = true, zeroes = true, 0, seqL - 1)
  }


  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val Array(b, l, r) = io.StdIn.readLine.split(" ").map(_.toLong)

      println(solve(r, b.toInt) - solve(l - 1, b.toInt))
    }
  }
}

