package codeforces.contest1197

/**
  * https://codeforces.com/contest/1197/problem/E
  *
  * Process R values in sorted order, saving result for each
  * Consider rooftop R values and sum each with the minimum space
  */
object CultureCode {
  import scala.collection.mutable

  val MOD: Long = 1000000000 + 7

  type Space = Int
  type Result = Long
  type R = Int
  type L = Int

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    val pairsMap: mutable.TreeMap[R, mutable.HashMap[L, Int]] = // sorted By r
      collection.mutable.TreeMap[R, collection.mutable.HashMap[L, Int]]()

    for (_ <- 1 to n) {
      val Array(upper, lower) = io.StdIn.readLine().split(" ").map(_.toInt)
      val lowerMap = pairsMap.getOrElseUpdate(upper, mutable.HashMap())
      lowerMap.update(lower, lowerMap.getOrElse(lower, 0) + 1)
    }

    val lastL: L = pairsMap.last._2.maxBy(_._1)._1

    @scala.annotation.tailrec
    def loop(ls: List[(R, mutable.HashMap[L, Int])],
             rValues: List[Int]): List[Int] = {
      ls match {
        case Nil => rValues
        case (r, lMap) :: tail =>
          val l = lMap.maxBy(_._1)._1
          if (lastL < l)
            r :: rValues
          else
            loop(tail, r :: rValues)
      }
    }

    val rValuesToConsider =
      loop(pairsMap.from(lastL + 1).toList.reverse, List())

    implicit val ordering: Ordering[(R, Space, Result)] =
      (x: (R, Space, Result), y: (R, Space, Result)) => x._1 - y._1

    val doneRValues =
      collection.mutable.TreeMap[R, (Space, Result)](1 -> (1, 1))

    def findLowerBound(r: Int): (R, (Space, Result)) =
      doneRValues.until(r + 1).last

    for ((r, lSeq) <- pairsMap) {
      val lastR: (R, (Space, Result)) = doneRValues.last
      (doneRValues.put _).tupled {

        var minSpace = lastR._2._1 + r - lastR._1
        var minSpaceResult: Result = lastR._2._2

        for ((l, pairOccurrences) <- lSeq) {
          val (prevR, (prevSpace, prevSpaceResult)) = findLowerBound(l)

          val space = prevSpace + l - prevR
          val spaceResult = (prevSpaceResult * pairOccurrences) % MOD

          if (minSpace == space)
            minSpaceResult = (minSpaceResult + spaceResult) % MOD
          else if (space < minSpace) {
            minSpace = space
            minSpaceResult = spaceResult
          }
        }

        r -> (minSpace, minSpaceResult)
      }
    }

    val (_, res) = {
      var (minSpace, minSpaceResult) = (Int.MaxValue, 0L)

      for {
        r <- rValuesToConsider
        (space, spaceResult) = doneRValues(r)
      } {
        if (minSpace == space)
          minSpaceResult = (minSpaceResult + spaceResult) % MOD
        else if (space < minSpace) {
          minSpace = space
          minSpaceResult = spaceResult
        }
      }

      (minSpace, minSpaceResult)
    }

    println(res)
  }
}
