package coursera

import scala.collection.mutable

object CoinChange {

  case class Pair(money: Int, coins: Int)

  val cache: mutable.Map[Pair, Long] = collection.mutable.Map[Pair, Long]()

  def count(money: Int, denominations: List[Int]): Long = {
    if (denominations.isEmpty) 0 else {
      val pair = Pair(money, denominations.length)
      if (cache.contains(pair)) cache(pair)
      else {
        val result = money.signum match {
          case 0 => 1
          case -1 => 0
          case 1 => count(money, denominations.tail) + count(money - denominations.head, denominations) // exclude + include
        }
        cache(pair) = result
        result
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val Array(m, n) = io.StdIn.readLine.split(" ").map(_.toInt)
    val denominations = io.StdIn.readLine.split(" ").map(_.toInt).toList

    println(count(m, denominations))
  }

}
