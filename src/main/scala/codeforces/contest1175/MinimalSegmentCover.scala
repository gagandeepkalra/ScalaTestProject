package codeforces.contest1175

/*
https://codeforces.com/contest/1175/problem/E

Given n ranges and  m queries in form [𝑥;𝑦]. What is the minimal number of intervals you have to take so that every point
(not necessarily integer) from 𝑥 to 𝑦 is covered by at least one of them?

We do this `Path Compression` style, point every no. in range to an interval and keep updating that interval with the farthest range.
We answer queries offline sorted by R values.

Many points correspond to one single interval, it cannot go functional this way, introduce a middleware, the index
 */
object MinimalSegmentCover {

  type Index = Int
  type L = Int
  type R = Int
  type Result = Int

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val intervalToIndexMap = new java.util.HashMap[L, (R, Index)]()
    val indexToResultMap = new java.util.HashMap[Index, (R, Result)]()

    val maxR: Int = (0 until n)
      .foldLeft(Int.MinValue) { (maximumR, index) =>
        val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)

        val (yMax, indexMax) = Option(
          intervalToIndexMap
            .get(x)
        ).map {
            case (currY, currIndex) =>
              if (y > currY) {
                indexToResultMap.remove(currIndex)
                (y, index)
              } else
                (currY, currIndex)
          }
          .getOrElse((y, index))

        intervalToIndexMap.put(x, (yMax, indexMax))
        indexToResultMap.put(indexMax, (yMax, 1))

        y max maximumR
      }

    val valueToRightmostIntervalsIndex: Array[Option[Index]] =
      Array.fill(maxR + 1)(None)

    (0 to maxR)
      .foldLeft(Option.empty[(R, Index)]) {
        case (prev, i) =>
          val current: Option[(R, Index)] = Option(intervalToIndexMap.get(i))

          val newPrev = current
            .flatMap {
              case c @ (cr, ci) =>
                prev.map {
                  case p @ (pr, _) =>
                    if (cr > pr) c
                    else {
                      indexToResultMap.remove(ci)
                      p
                    }
                }
            }
            .orElse(current)
            .orElse(prev)
            .filter(_._1 >= i)

          newPrev.foreach {
            case (_, index) => valueToRightmostIntervalsIndex(i) = Some(index)
          }
          newPrev
      }

    def pathCompression(l: L, r: R): Option[(R, Result)] = // attempt your best to go right
      for {
        lIndex <- valueToRightmostIntervalsIndex(l)
        (rightmost, steps) <- Option(indexToResultMap.get(lIndex))

        rightmostResult = if (rightmost < r) {
          if (rightmost != l)
            pathCompression(rightmost, r)
              .map {
                case (rightmostR, stepsR) =>
                  val res = (rightmostR, steps + stepsR)
                  indexToResultMap.put(lIndex, res)
                  res
              } else None
        } else Some(rightmost, steps)

        (maxRightmost, minSteps) <- rightmostResult match {
          case None =>
            indexToResultMap.remove(lIndex)
            None
          case s @ Some((_, _)) => s
        }
      } yield (maxRightmost, minSteps)


    // safe
    val queries: Array[(L, R)] = {
      val arr = new Array[(L, R)](m)
      (0 until m)
        .foreach { i =>
          val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
          arr(i) = (x, y)
        }
      arr
    }

    val resArrayM: Array[Result] = Array.fill(m)(-1)

    (0 until m).toArray
      .sortBy(index => queries(index)._2)
      .foreach { qi =>
        val (ql, qr) = queries(qi)

        if (ql <= maxR && qr <= maxR) {
          pathCompression(ql, qr)
            .foreach { case (_, steps) => resArrayM(qi) = steps }
        }
      }

    println(resArrayM.mkString("\n"))

  }

}
