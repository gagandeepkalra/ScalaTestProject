class RandomaizeArray(_nums: Array[Int]) {

  /** Resets the array to its original configuration and return it. */
  def reset(): Array[Int] = _nums

  /** Returns a random shuffling of the array. */
  def shuffle(): Array[Int] = {
    val result = _nums map identity
    for (i <- result.indices) {
      val j = scala.util.Random.nextInt(result.length)
      if (i != j) {
        result(i) = result(i) ^ result(j)
        result(j) = result(i) ^ result(j)
        result(i) = result(i) ^ result(j)
      }
    }
    result
  }

}
