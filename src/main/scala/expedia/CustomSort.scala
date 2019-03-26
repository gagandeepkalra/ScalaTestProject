package expedia

import scala.io.StdIn

object CustomSort {

  /*
   * Complete the 'customSort' function below.
   *
   * The function accepts INTEGER_ARRAY arr as parameter.
   */

  def customSort(arr: Array[Int]) {

    arr.groupBy(identity).toArray
      .map { case (value, copies) => (copies.length, value) }
      .sorted
      .flatMap { case (freq, value) => Array.fill(freq)(value) }
      .foreach(println)

  }

}

object Solution {
  def main(args: Array[String]) {
    val arrCount = StdIn.readLine.trim.toInt

    val arr = Array.ofDim[Int](arrCount)

    for (i <- 0 until arrCount) {
      val arrItem = StdIn.readLine.trim.toInt
      arr(i) = arrItem
    }

    CustomSort.customSort(arr)
  }
}