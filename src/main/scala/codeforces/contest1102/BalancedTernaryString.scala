package codeforces.contest1102

object BalancedTernaryString {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt
    val s = io.StdIn.readLine

    var zeroesToReplace = n / 3 - s.count(_ == '0')
    var onesToReplace = n / 3 - s.count(_ == '1')
    var twosToReplace = n / 3 - s.count(_ == '2')

    var zeroesCounted, onesCounted, twosCounted = 0

    val res = s.toCharArray
    res.indices.foreach(i => {
      res(i) match {
        case '0' =>
          zeroesCounted += 1
          if (zeroesCounted > n / 3) {
            if (onesToReplace < 0) {
              res(i) = '1'
              onesToReplace += 1
            } else if (twosToReplace < 0) {
              res(i) = '2'
              twosToReplace += 1
            }
          }
        case '1' =>
        case '2' =>
      }
    })


  }

}
