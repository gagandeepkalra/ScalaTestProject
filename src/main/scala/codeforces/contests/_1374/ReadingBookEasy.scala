package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/E1

Alice should like at least k books,
Bob should like at least k books,
and then we have to minimise the sum total

we split into three sets, alice, bob and both, afterwards we find minimum by selecting 0 then 1 then 2.. elements from
both and relevant from alice and bob
 */
object ReadingBookEasy {

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val (aliceL, bobL, bothL) = (1 to n).foldLeft((List.empty[Int], List.empty[Int], List.empty[Int])) {
      case ((alice, bob, both), _) =>
        val Array(time, a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
        (
          if (a == 1 && b == 0) time :: alice else alice,
          if (a == 0 && b == 1) time :: bob else bob,
          if (a == 1 && b == 1) time :: both else both
        )
    }

    val aliceA = aliceL.toArray
    val bobA = bobL.toArray
    val bothA = bothL.toArray

    println {
      if (aliceA.length + bothA.length >= k && bobA.length + bothA.length >= k) {

        val aliceP = aliceA.sorted.scanLeft(0)(_ + _)
        val bobP = bobA.sorted.scanLeft(0)(_ + _)
        val bothP = bothA.sorted.scanLeft(0)(_ + _)

        val i = bothP.indices.minBy { i =>
          if (i > k) Int.MaxValue
          else {
            val r = k - i
            if (r < aliceP.length && r < bobP.length)
              aliceP(r) + bobP(r) + bothP(i)
            else Int.MaxValue
          }
        }
        val r = k - i
        aliceP(r) + bobP(r) + bothP(i)
      } else -1
    }
  }
}
