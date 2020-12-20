package GoogleCodeJam2019RoundB

object BuildingPalindromes {

  def main(args: Array[String]): Unit = {
    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {
      val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)

      val sequence = io.StdIn.readLine()

      val arr = Array.ofDim[Int](n, 26)


      val temp = new Array[Int](26)

      temp.clone()
      for (i <- 0 until n) {

        temp(sequence(i) - 'A') += 1

        arr(i) = temp.map(identity)
      }

      var result = 0

      for (_ <- 1 to q) {
        val Array(l, r) = io.StdIn.readLine.split(" ").map(_.toInt).map(_ - 1)

        def isPalindrome(a1: Int, a2: Int) = {
          val oddCount = (
            if (a1 >= 0)
              arr(a1).zip(arr(a2)).map { case (x, y) => y - x }
            else
              arr(a2)
            )
            .count(_ % 2 == 1)
          oddCount == 0 || oddCount == 1

        }

        if (isPalindrome(l - 1, r)) result += 1
      }

      println(s"Case #$t: $result")


    }
  }

}
