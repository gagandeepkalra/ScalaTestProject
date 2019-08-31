package codeforces.contest1213

/*
https://codeforces.com/contest/1213/problem/D1
https://codeforces.com/contest/1213/problem/D2
 */
object EqualizingByDivision {

  def main(args: Array[String]): Unit = {
    val Array(_, k) = io.StdIn.readLine().split(" ").map(_.toInt)
    val elements = io.StdIn.readLine().split(" ").map(_.toInt)

    val maxElement = elements.max

    val frequency = Array.ofDim[Int](maxElement + 1, 20)

    for (e <- elements) {
      var temp = e
      var i = 0
      while (temp > 0) {
        frequency(temp)(i) += 1

        i += 1
        temp = temp / 2
      }
    }

    println {
      (1 to maxElement).foldLeft(Int.MaxValue) {
        case (acc, element) =>
          val (steps, equals) = (0 until 20).foldLeft((0, 0)) {
            case ((steps, equals), i) =>
              val c = frequency(element)(i)
              if (c != 0 && equals < k)
                if (equals + c <= k) (steps + i * c, equals + c)
                else (steps + i * (k - equals), k)
              else
                (steps, equals)
          }

          if (equals == k) acc min steps else acc
      }
    }
  }
}
