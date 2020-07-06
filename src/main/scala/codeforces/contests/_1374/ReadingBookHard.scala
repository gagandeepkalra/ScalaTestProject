package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/E2

Alice should read at least k books,
Bob should like at least k books,
and then we have to minimise the sum total

but this time an additional constraint that there be exactly m books read in total.

we max out contribution from Both first, if unable to satisfy the `k` constraint we borrow equal from Alice and Bob,
short if we cant or in doing so if we exceeded m.
Afterwards the aim is to reach m books with the minimum sum total, we do that one at a time, trying to add as low as possible-
1. if replacing both.top with alice and bob is the lowest
2. else we select minimum of the four channels

The idea is we start at a respectable position satisfying the `k` constraint, treading slowly then
 */
object ReadingBookHard {

  def main(args: Array[String]): Unit = {
    val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val (aliceL, bobL, bothL, noneL) = (1 to n).foldLeft((List.empty[(Int, Int)], List.empty[(Int, Int)], List.empty[(Int, Int)], List.empty[(Int, Int)])) {
      case ((alice, bob, both, none), i) =>
        val Array(time, a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
        (
          if (a == 1 && b == 0) (time, i) :: alice else alice,
          if (a == 0 && b == 1) (time, i) :: bob else bob,
          if (a == 1 && b == 1) (time, i) :: both else both,
          if (a == 0 && b == 0) (time, i) :: none else none
        )
    }

    val aliceA = aliceL.toArray.sortBy(_._1)
    val bobA = bobL.toArray.sortBy(_._1)
    val bothA = bothL.toArray.sortBy(_._1)
    val noneA = noneL.toArray.sortBy(_._1)

    val c = bothA.length min k // next pos
    val a, b = k - c // next pos

    if (a > aliceA.length || b > bobA.length || a + b + c > m)
      println(-1)
    else {
      @scala.annotation.tailrec
      def loop(a: Int, b: Int, c: Int, d: Int, mi: Int = 0): (Int, Int, Int, Int) = {
        if (mi == m) (a, b, c, d)
        else {
          if (0 < c && a < aliceA.length && b < bobA.length && {
            val delta = aliceA(a)._1 + bobA(b)._1 - bothA(c - 1)._1
            !(delta > aliceA(a)._1) && !(delta > bobA(b)._1) && !(c < bothA.length && delta > bothA(c)._1) && !(d < noneA.length && delta > noneA(d)._1)
          })
            loop(a + 1, b + 1, c - 1, d, mi + 1)
          else if (a < aliceA.length && !(b < bobA.length && aliceA(a)._1 > bobA(b)._1) && !(c < bothA.length && aliceA(a)._1 > bothA(c)._1) && !(d < noneA.length && aliceA(a)._1 > noneA(d)._1))
            loop(a + 1, b, c, d, mi + 1)
          else if (b < bobA.length && !(a < aliceA.length && bobA(b)._1 > aliceA(a)._1) && !(c < bothA.length && bobA(b)._1 > bothA(c)._1) && !(d < noneA.length && bobA(b)._1 > noneA(d)._1))
            loop(a, b + 1, c, d, mi + 1)
          else if (c < bothA.length && !(a < aliceA.length && bothA(c)._1 > aliceA(a)._1) && !(b < bobA.length && bothA(c)._1 > bobA(b)._1) && !(d < noneA.length && bothA(c)._1 > noneA(d)._1))
            loop(a, b, c + 1, d, mi + 1)
          else if (d < noneA.length && !(a < aliceA.length && noneA(d)._1 > aliceA(a)._1) && !(b < bobA.length && noneA(d)._1 > bobA(b)._1) && !(c < bothA.length && noneA(d)._1 > bothA(c)._1))
            loop(a, b, c, d + 1, mi + 1)
          else
            (a, b, c, d)
        }
      }

      val (fai, fbi, fci, fdi) = loop(a, b, c, 0, a + b + c)

      val faiIncluded = (0 until fai).foldLeft((0, List.empty[Int])) { case ((res, indices), i) =>
        (res + aliceA(i)._1, aliceA(i)._2 :: indices)
      }

      val fbiIncluded = (0 until fbi).foldLeft(faiIncluded) { case ((res, indices), i) =>
        (res + bobA(i)._1, bobA(i)._2 :: indices)
      }

      val fciIncluded = (0 until fci).foldLeft(fbiIncluded) { case ((res, indices), i) =>
        (res + bothA(i)._1, bothA(i)._2 :: indices)
      }

      val fdiIncluded = (0 until fdi).foldLeft(fciIncluded) { case ((res, indices), i) =>
        (res + noneA(i)._1, noneA(i)._2 :: indices)
      }

      val (res, indices) = fdiIncluded

      println(res)
      println(indices.mkString(" "))
    }
  }
}
