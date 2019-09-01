package codeforces.contest1213

/*
https://codeforces.com/contest/1213/problem/E
 */
object TwoSmallStrings {
  def categoriseInput(l1: String,
                      r1: String,
                      l2: String,
                      r2: String): (Int, (String, String, String)) = {
    val all = Set("a", "b", "c")

    if (l1 == l2 && r1 == r2) { // both exactly same
      if (l1 == r1)
        (9, (l1, (all - l1).toList.head, (all - l1).toList.last))
      else
        (10, (l1, r1, (all - l1 - r1).head))
    } else if (l1 == l2) { // same first char
      if (l1 == r1)
        (1, (l1, r2, (all - l1 - r2).head))
      else if (l1 == r2)
        (1, (l1, r1, (all - l1 - r1).head))
      else
        (2, (l1, r1, r2))
    } else { // different first char
      if (l1 == r1 && l2 == r2) // double self
        (6, (l1, l2, (all - l1 - l2).head))
      else if (l1 == r1) { // single self
        if (l1 == r2)
          (5, (l1, l2, (all - l1 - l2).head))
        else
          (4, (l1, l2, r2))
      } else if (l2 == r2) { // single self
        if (l2 == r1)
          (5, (l2, l1, (all - l1 - l2).head))
        else
          (4, (l2, l1, r1))
      } else { // no self
        if (l2 == r1 && l1 == r2)
          (8, (l1, r1, (all - l1 - l2).head))
        else if (l2 == r1)
          (3, (l1, r1, r2))
        else if (l1 == r2)
          (3, (l2, r2, r1))
        else
          (7, (l1, r1, l2)) // r1 ==r2
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val Array(l1, r1) = io.StdIn.readLine.toCharArray.map(_.toString)
    val Array(l2, r2) = io.StdIn.readLine.toCharArray.map(_.toString)
    val (i, (x, y, z)) = categoriseInput(l1, r1, l2, r2)

    println(s"YES")
    println {
      i match {
        case 1 | 3 | 4 | 5 | 8 | 9 | 10 => (x + z) * n + y * n
        case 2                          => y * n + z * n + x * n
        case 6                          => (x + y) * n + z * n
        case 7                          => y * n + (x + z) * n
      }
    }
  }

}
