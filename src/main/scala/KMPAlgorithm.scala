object KMPAlgorithm {

  def computeLongestPrefixSuffixArray(pattern: String): Array[Int] = {
    val lps = new Array[Int](pattern.size)
    var (i, j) = (1, 0)

    while (i < pattern.size) {
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

  def search(text: String, pattern: String): Boolean = {
    val lps = computeLongestPrefixSuffixArray(pattern)
    var (i, j) = (0, 0)

    while (i < text.size) {

      if (j == pattern.size) return true
      else if (text(i) == pattern(j)) {
        i += 1
        j += 1
      }
      else if (j != 0) j = lps(j - 1)
      else i += 1

    }

    if (j == pattern.size) return true
    else false
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    (1 to n).map { _ =>
      val text = io.StdIn.readLine
      val pattern = io.StdIn.readLine
      search(text, pattern)
    }.foreach(res => if (res) println("YES") else println("NO"))
  }

}
