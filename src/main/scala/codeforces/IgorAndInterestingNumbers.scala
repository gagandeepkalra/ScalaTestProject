package codeforces

/*
Igor and Interesting Numbers

https://codeforces.com/problemset/problem/747/F

DP

more like rank of a permutation with an twist that leading zeroes don't count

we figure out weight of each path, then search from top but before we need to find top.

leading 0's don't count we start bottom up, previous layer is 0th weight of the next layer

 */
object IgorAndInterestingNumbers {

  val hex: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  val nCr: Array[Array[Int]] = {
    val result = (1 to 20).map(Array.fill(_)(1))

    // nCr = (n-1)Cr + (n-1)C(r-1), pascal's triangle
    for (i <- 2 until 20; j <- 1 until i) {
      result(i)(j) = result(i - 1)(j) + result(i - 1)(j - 1)
    }

    result.toArray
  }


  def countUniquePermutations(limit: Array[Int], spaces: Int): Int = {
    if (spaces == 0) 1
    else {
      val dp = Array.fill(spaces + 1)(new Array[Int](16))

      for (j <- 0 until 16) dp(0)(j) = 1

      for {
        i <- 1 to spaces
        _ = dp(i)(0) = if (limit(0) >= i) 1 else 0
        j <- 1 until 16
        if limit(j) >= 0
        k <- 0 to limit(j).min(i)
      } dp(i)(j) += dp(i - k)(j - 1) * nCr(i)(k)


      dp(spaces)(15)
    }
  }

  def findStartingDigit(rank: Int, t: Int): (Int, Int, Int) = {
    val state = Array.fill[Int](16)(t)

    def recur(rankLeft: Int, index: Int): Either[Int, (Int, Int)] = // return (Char index and rank left in that branch)
      (1 until 16).filter(state(_) > 0)
        .foldLeft[Either[Int, (Int, Int)]](Left(rankLeft))((eitherAcc, i) =>
          eitherAcc.fold(rankLeft => {
            state(i) -= 1
            val r = countUniquePermutations(state, index - 1)
            state(i) += 1
            if (r >= rankLeft)
              Right((i, rankLeft))
            else
              Left(rankLeft - r)
          }, Right(_))
        )

    def loop(rankLeft: Int, index: Int): (Int, Int, Int) = { // returns Char index, index, rankLeft
      recur(rankLeft, index).fold(loop(_, index + 1), c => (c._1, index, c._2))
    }

    loop(rank, 1)
  }

  def solveByDivideAndConquer(k: Int, t: Int): String = {
    val state = Array.fill[Int](16)(t)

    def recur(index: Int, rankLeft: Int): List[Char] = {
      if (index == 0) {
        Nil
      } else
        (0 until 16)
          .filter(state(_) > 0)
          .foldLeft[Either[Int, List[Char]]](Left(rankLeft))(
            (either, i) =>
              either
                .fold(rLeft => {
                  state(i) -= 1
                  val r = countUniquePermutations(state, index - 1)
                  val either = if (r >= rLeft) Right(hex(i) :: recur(index - 1, rLeft)) else Left(rLeft - r)
                  state(i) += 1
                  either
                }, Right(_))
          ).getOrElse(Nil)
    }

    val (firstCharIndex, index, rankLeft) = findStartingDigit(k, t)

    state(firstCharIndex) -= 1

    (hex(firstCharIndex) :: recur(index - 1, rankLeft)).mkString
  }

  def main(args: Array[String]): Unit = {
    val Array(k, t) = io.StdIn.readLine.split(" ").map(_.toInt)

    println(solveByDivideAndConquer(k, t))
  }

  def bruteRecursive(k: Int, t: Int): String = {
    val state = Array.fill[Int](16)(t)

    // digit dp, recursive
    def recurToFindKth(index: Int, rank: Int, includeZero: Boolean = true): Either[Int, List[Char]] = {
      if (index == 0) {
        if (rank + 1 == k) Right(Nil) else Left(rank + 1)
      }
      else
        ((if (includeZero) 0 else 1) until 16)
          .foldLeft(Left(rank): Either[Int, List[Char]]) {
            case (Left(acc), s) =>
              if (state(s) > 0) {
                state(s) -= 1
                val updated = recurToFindKth(index - 1, acc)
                state(s) += 1
                updated.map(hex(s) :: _)
              } else Left(acc)
            case (right, _) => right
          }
    }

    def find(index: Int, rank: Int = 0): String = {
      recurToFindKth(index, rank, includeZero = false).fold(find(index + 1, _), _.mkString)
    }

    find(1)
  }

}
