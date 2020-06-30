package hackerrank

/*
remove m items from the given array so as to reduce the no. of distinct elements
 */
object DeleteProducts {
  def deleteProducts(ids: Array[Int], m: Int): Int = {
    val frequencies: Array[(Int, Int)] = ids.groupBy(identity).mapValues(_.length).toArray.sortBy(_._2)

    frequencies.foldLeft((m, 0)) { case ((accM, ans), (_, v)) =>
      if (accM == 0) (0, ans + 1)
      else if (accM < v) (0, 1)
      else (accM - v, 0)
    }._2
  }

  def main(args: Array[String]): Unit = {
    println(deleteProducts(Array(1, 1, 1, 2, 3, 3), 2))
    println(deleteProducts(Array(1, 1, 5, 5), 2))
  }
}
