package GoogleCodeJamRoundD

object Q1 {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      var Array(n, o, d) = io.StdIn.readLine.split(" ").map(_.toLong)
      val Array(x1, x2, a, b, c, m, l) = io.StdIn.readLine.split(" ").map(_.toLong)

      val arrX = new Array[Long](n.toInt + 1)
      var arrS = new Array[Long](n.toInt + 1)

      arrX(1) = x1
      arrS(1) = arrX(1) + l

      arrX(2) = x2
      arrS(2) = arrX(2) + l

      for (i <- 3 to n.toInt) {
        arrX(i) = (a * arrX(i - 1) % m + b * arrX(i - 2) + c) % m
        arrS(i) = arrX(i) + l
      }

      var wasDnegative = false
      if (d < 0) {
        arrS = arrS.map(-_)
        d *= -1
        wasDnegative = true
      }

      var maxSoFar: Long = Long.MinValue
      var currentMax: Long = 0

      var oddCount: Long = 0

      var i = 1
      var j = 1

      while (j <= n) {

        do {
          currentMax = if (arrS(j) >= currentMax + arrS(j)) { // decide
            i = j
            oddCount = arrS(j) % 2
            arrS(j)
          } else {
            oddCount = oddCount + arrS(j) % 2
            currentMax + arrS(j)
          }

          if (oddCount <= o && currentMax <= d) {
            maxSoFar = math.max(maxSoFar, currentMax)
          }

          j += 1
        } while (j <= n && oddCount <= o && currentMax <= d)

        do {
          currentMax -= arrS(i)
          oddCount = oddCount - arrS(i) % 2
          i += 1
        } while (i < j && (oddCount > o || currentMax > d))

        if (i < j) {
          maxSoFar = math.max(maxSoFar, currentMax)
        }
      }

      if (wasDnegative) maxSoFar *= -1

      System.out.println(s"Case #$t: " + (if (maxSoFar != Long.MinValue) maxSoFar else "IMPOSSIBLE"))
    }
  }
}
