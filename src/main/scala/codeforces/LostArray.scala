package codeforces

object LostArray {
  def computeLongestPrefixSuffixArray(pattern: Array[Int]): Array[Int] = {
    val lps = new Array[Int](pattern.length)
    var (i, j) = (1, 0)

    while (i < pattern.length) {
      if (pattern(i) == pattern(j)) {
        j += 1
        lps(i) = j
        i += 1
      } else {
        if (j != 0) {
          j = lps(j - 1)
        }
        else {
          i += 1
        }
      }
    }

    lps
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt
    val arr = io.StdIn.readLine.split(" ").map(_.toInt)

    val b = new Array[Int](n)
    b(0) = arr(0)
    var i = 1
    while (i < n) {
      b(i) = arr(i) - arr(i - 1)
      i += 1
    }

    val lps = computeLongestPrefixSuffixArray(b)

    val factor = n - lps(n - 1)

    var res = List[Int]()

    i = factor
    while (i < n) {
      res = i :: res
      i += factor
    }
    res = n :: res

    res = res.reverse
    println(res.size)
    println(res.mkString(" "))
  }
}
