package hackerrank.WeekOfCode37

import scala.io.StdIn

object DynamicLineIntersection {

  def computePrimes(n: Int): Array[Int] = {
    val factors = new Array[Int](n + 1)

    (2 to n).foreach { i =>
      if (factors(i) == 0) {
        (2 * i to n by i).foreach { j =>
          factors(j) = i
        }
      }
    }

    factors
  }

  def getFactorsFrequencyMap(n: Int, factors: Array[Int]): collection.mutable.Map[Int, Int] = {
    val store = collection.mutable.Map[Int, Int]()

    var i = n
    while (factors(i) != 0) {
      store(factors(i)) = store.getOrElse(factors(i), 0) + 1
      i = i / factors(i)
    }
    store(factors(i)) = store.getOrElse(factors(i), 0) + 1

    store
  }


  def dynamicLineIntersection(n: Int) {
    val store = collection.mutable.Map[Int, collection.mutable.Map[Int, Int]]()

    (1 to n).foreach(_ => {
      StdIn.readLine.split(" ") match {
        case Array("+", mm, cc) => {
          val m = mm.toInt
          val c = cc.toInt % m

          store(m) = store.getOrElse(m, collection.mutable.Map[Int, Int]())
          store(m)(c) = store(m).getOrElse(c, 0) + 1

        }
        case Array("-", mm, cc) => {
          val m = mm.toInt
          val c = cc.toInt % m

          if (store(m)(c) == 1) {
            store(m).remove(c)
          } else store(m)(c) = store(m)(c) - 1

          if (store(m).isEmpty) store.remove(m)

        }
        case Array("?", yy) => {
          val y = yy.toInt

          println(store.map(pair => { // for each slope
            pair._2.getOrElse(y % pair._1, 0)
          }).sum)
        }
      }
    })


  }

  def main(args: Array[String]) {


    val n = StdIn.readLine.trim.toInt

    dynamicLineIntersection(n)
  }

}
