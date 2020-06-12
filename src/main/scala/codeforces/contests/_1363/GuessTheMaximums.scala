package codeforces.contests._1363

/*
https://codeforces.com/contest/1363/problem/D

[Binary search]

The problem reduces to two cases where a. the maximum is part of any subset or b. it's not
 */
object GuessTheMaximums {

  def ask(seq: Traversable[Int]): Int = {
    val c = seq.size

    println(s"? $c ${seq.mkString(" ")}")
    System.out.flush()

    io.StdIn.readInt()
  }

  def findIndexOfMaximumElement(n: Int): (Int, Int) = {
    val maxValue = ask(1 to n)

    @scala.annotation.tailrec
    def binarySearch(start: Int, end: Int): Int = {
      if (start == end) start
      else {
        val mid = (start + end) / 2

        val key = ask(start to mid)

        if (key == maxValue) binarySearch(start, mid) else binarySearch(mid + 1, end)
      }
    }

    (binarySearch(1, n), maxValue)
  }

  def solve: Boolean = {
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
    val subsets = {
      val arr = new Array[Set[Int]](k)
      for (i <- 0 until k) {
        val indices = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(i) = indices.view.tail.toSet
      }
      arr
    }

    val (idx, max) = findIndexOfMaximumElement(n)

    println {
      s"! ${subsets.map(set => if (set(idx)) ask((1 to n).toSet -- set) else max).mkString(" ")}"
    }

    io.StdIn.readLine == "Correct"
  }

  def main(args: Array[String]): Unit = {
    val t = io.StdIn.readInt()
    (1 to t).takeWhile(_ => solve)
  }
}
