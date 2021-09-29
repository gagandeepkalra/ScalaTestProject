package google.kickstart._2021.C

object SmallerStrings {

  val MOD = 1000000007

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val string      = io.StdIn.readLine

      val PalindromicSize = (n + 1) / 2

      val powerLookup: IndexedSeq[Long] = {
        val lookup = new Array[Long](PalindromicSize)
        lookup.indices.foldRight(k) { (i, acc) =>
          lookup(i) = acc
          (acc * k) % MOD
        }

        lookup
      }

      @scala.annotation.tailrec
      def loop(acc: Long = 0L, index: Int = 0): Long = {
        index match {
          case i if i == PalindromicSize => acc
          case i if i == PalindromicSize - 1 =>
            val diff = string(i) - 'a'
            loop((acc + diff) % MOD, index + 1)
          case i =>
            val diff = string(i) - 'a'
            loop((acc + (diff * powerLookup(i + 1)) % MOD) % MOD, index + 1)
        }
      }

      val currentPalindrome = {
        if (n % 2 == 0) {
          val first = string.take(n / 2)

          first + first.reverse
        } else {
          val first = string.take(n / 2)
          val mid   = string(n / 2).toString

          first + mid + first.reverse
        }
      }

      val includeS = currentPalindrome < string

      printFormattedOutput(t, (loop() + (if (includeS) 1 else 0)) % MOD)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
