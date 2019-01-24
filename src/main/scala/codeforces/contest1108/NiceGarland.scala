package codeforces.contest1108

/*
http://codeforces.com/contest/1108/problem/C
 */
object NiceGarland {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val str = io.StdIn.readLine()

    val maxZeroes = n / 3 + (if (n % 3 >= 1) 1 else 0)
    val maxOnes = n / 3 + (if (n % 3 == 2) 1 else 0) // 1 2 3, 4 5 6, 7 8 9
    val maxTwos = n / 3

    def count(c: Char): Array[Int] = {
      Array(maxZeroes - (0 until n by 3).count(str(_) == c), maxOnes - (1 until n by 3).count(str(_) == c), maxTwos - (2 until n by 3).count(str(_) == c))
    }

    val redCost = count('R')
    val blueCost = count('B')
    val greenCost = count('G')

    val (optimalPermutation: String, cost: Int) = Set[String]("RGB", "RBG", "BGR", "BRG", "GRB", "GBR")
      .map(s =>
        (s, s.zipWithIndex.foldLeft(0) {
            case (acc: Int, (c: Char, i: Int)) =>
              acc + (c match {
                case 'R' => redCost(i)
                case 'B' => blueCost(i)
                case 'G' => greenCost(i)
              })
          })).minBy(_._2)

    val result = str.toCharArray

    optimalPermutation.zipWithIndex.foreach { case (c, i) => fix(i, c) }

    def fix(startingFrom: Int, withColor: Char): Unit = {
      (startingFrom until n by 3).foreach(result(_) = withColor)
    }

    println(cost + " " + result.mkString(""))
  }
}
