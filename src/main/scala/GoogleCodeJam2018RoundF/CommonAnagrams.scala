//package GoogleCodeJam2018RoundF

object CommonAnagrams {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {

      val l = io.StdIn.readInt()
      val a, b = io.StdIn.readLine()

      val temp = new Array[Int](26)

      // all substrings of a

      printFormattedOutput(t, (0 until l).map { i =>
        var res = 0

        def recur(j: Int): Unit = {
          if (j != l) {
            temp(a(j) - 'A') += 1

            if (check(b, temp, j - i + 1)) res += 1

            recur(j + 1)
            temp(a(j) - 'A') -= 1
          }
        }

        recur(i)
        res
      }.sum)

    }
  }

  def check(b: String, temp: Array[Int], subStringLength: Int): Boolean = {
    b.sliding(subStringLength).toStream.exists(sub => toFrequencyArray(sub).deep == temp.deep)
  }

  def toFrequencyArray(b: String): Array[Int] = {
    val frequency = new Array[Int](26)
    b.foreach(c => frequency(c - 'A') += 1)
    frequency
  }


  def printFormattedOutput(i: Int, op: Int): Unit = {
    println(s"Case #$i: $op")
  }

}
