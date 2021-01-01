package adventOfCode

import scala.annotation.tailrec

/**
 * part: 01
 *
 * Your notes (your puzzle input) consist of two lines. The first line is your estimate of the earliest timestamp you
 * could depart on a bus. The second line lists the bus IDs that are in service according to the shuttle company; entries
 * that show x must be out of service, so you decide to ignore them.
 *
 * To save time once you arrive, your goal is to figure out the earliest bus you can take to the airport. (There will be exactly one such bus.)
 *
 * part: 02
 *
 * The shuttle company is running a contest: one gold coin for anyone that can find the earliest timestamp such that the
 * first bus ID departs at that time and each subsequent listed bus ID departs at that subsequent minute.
 * (The first line in your input is no longer relevant.)
 *
 * What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?
 */
object _13_ShuttleSearch {

  // part: 01
  def findTime(earliest: Int, id: Int): Int = {
    if (earliest % id == 0) earliest
    else id * (earliest / id + 1)
  }

  def findClosestWaitTime(earliest: Int, ids: Seq[Int]): Int = {
    val (id, depart) = ids.map(id => id -> findTime(earliest, id)).minBy(_._2)
    id * (depart - earliest)
  }

  // part: 02
  def extendedEuclideanAlgorithm(a: Long, b: Long): (Long, Long, Long) = { // return x, y, gcd
    if (b == 0) (1, 0, a)
    else {
      val (x1, y1, gcd) = extendedEuclideanAlgorithm(b, a % b)

      (y1, x1 - (a / b) * y1, gcd)
    }
  }

  @tailrec
  def modInverseNaive(a: Long, m: Long, i: Long = 1): Long =
    if (a * i % m == 1) i else modInverseNaive(a, m, i + 1)

  def modInverse(a: Long, m: Long): Long = {
    val (x, _, gcd) = extendedEuclideanAlgorithm(a, m)

    if (gcd != 1)
      modInverseNaive(a, m)
    else
      (x % m + m) % m
  }

  def chineseRemainderTheorem(nums: Seq[Long], rem: Seq[Long]): Long = {
    val n = nums.length

    val allProduct = nums.product
    val product = nums.map(allProduct / _)

    val inverses = (0 until n).map(i => modInverse(product(i), nums(i)))

    val sum = (0 until n)
      .map(i => 1L * rem(i) * product(i) * inverses(i))
      .sum

    sum % allProduct
  }

  def main(args: Array[String]): Unit = {
    val input = """1008833
                  |19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,643,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,23,x,x,x,x,x,x,x,509,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29""".stripMargin
      .split("\n")

    val depart: Int = input(0).toInt
    val ids = input(1).split(",")
    val validIds: Array[Int] =
      input(1).split(",").collect { case v if v != "x" => v.toInt }

    println(findClosestWaitTime(depart, validIds))
    println {
      val pattern: Vector[(Long, Long)] = ids.reverse.zipWithIndex.flatMap {
        case ("x", _) => None
        case (v, i)   => Some(v.toLong -> i.toLong)
      }.toVector

      val (nums, rems) = pattern.unzip

      chineseRemainderTheorem(nums, rems) - ids.length + 1
    }
  }
}
