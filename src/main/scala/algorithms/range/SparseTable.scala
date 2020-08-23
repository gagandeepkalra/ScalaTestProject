package algorithms.range

/**
 * Sparse Table for O(1) range queries, stores indices
 *
 * Sparse Table is a data structure, that allows answering range queries. It can answer most range queries in O(logn),
 * but its true power is answering range minimum queries (or equivalent range maximum queries). For those queries it
 * can compute the answer in O(1) time.
 *
 * The only drawback of this data structure is, that it can only be used on immutable arrays. This means, that the array
 * cannot be changed between two queries. If any element in the array changes, the complete data structure has to be recomputed.
 *
 * @param n max index exclusive
 * @param combine function2 to describe reduction, takes two indices and returns an index
 */
class SparseTable(n: Int, combine: (Int, Int) => Int) {
  private val dp: Array[Array[Int]] = {
    val rows: Int = n
    val columns: Int = (math.log(rows) / math.log(2)).ceil.toInt + 1

    val dp: Array[Array[Int]] = Array.ofDim[Int](rows, columns)
    (0 until rows).foreach(i => dp(i)(0) = i)

    var j = 1
    while (j < columns) {
      var i = 0
      while (i + (1 << j) <= n) {
        dp(i)(j) = combine(dp(i)(j - 1), dp(i + (1 << (j - 1)))(j - 1))
        i += 1
      }
      j += 1
    }

    dp
  }

  def query(l: Int, r: Int): Int = { // both inclusive
    val j = (math.log(r - l + 1) / math.log(2)).floor.toInt
    combine(dp(l)(j), dp(r - (1 << j) + 1)(j))
  }
}
