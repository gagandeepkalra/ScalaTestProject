package google.kickstart._2019.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edb/00000000001707b8

[Binary Search]

given slot value equivalence, a and b, then we have 1 unit of first equals a/b units of second; sort by lower per unit values
e.g between 3 5 and 3 10 we'd choose 3 5

after sorting, we produce scan left cumulative collection then binary search to find a afterwards check against cumulative suffix for b

e.g. slots-
6 10
3 8

sequence-
0 18
6 8
9 0
 */
object CodeEatSwitcher {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(d, s) = io.StdIn.readLine.split(" ").map(_.toInt)

      val slots = {
        (1 to s)
          .map { _ => val Array(a, b) = io.StdIn.readLine.split(" ").map(_.toInt); (a, b) }
          .sortBy[Double] { case (a, b) => (b * 1.0) / a }
      }

      val scanned = slots.scanLeft(0)(_ + _._1) zip slots.scanRight(0)(_._2 + _)

      @scala.annotation.tailrec
      def upperBoundBinarySearch(l: Int, r: Int)(implicit a: Int): Int = {
        if (l == r) l
        else {
          val m = (l + r) / 2
          val key = scanned(m)._1

          if (a == key) m
          else if (key < a) upperBoundBinarySearch(m + 1, r)
          else upperBoundBinarySearch(l, m) // (key < a)
        }
      }

      def checkIfDoable(a: Int, b: Int): Boolean = {
        val l = 0
        val r = scanned.length - 1

        a <= scanned(r)._1 && {
          val i = upperBoundBinarySearch(l, r)(a)
          val (aCumulative, bRCumulative) = scanned(i)
          if (i > 0) {
            val (aActual, bActual) = slots(i - 1)

            val f = ((aCumulative - a) * 1.0) / aActual
            val bRemaining = f * bActual + bRCumulative

            bRemaining >= b
          } else bRCumulative >= b // i == 0
        }

      }

      val result = (1 to d).foldLeft(new StringBuilder) { (acc, _) =>
        val Array(a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
        acc.append(if (checkIfDoable(a, b)) 'Y' else 'N')
      }.toString


      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
