package algorithms.arrays

object Stack {

  /**
   * Monotonic Stack, left view
   *
   * @param seq elements, randomly accessible
   * @param ordering implicit ordering for gt operator
   * @tparam A type of each element
   * @return left view
   */
  def leftView[A](seq: IndexedSeq[A])(implicit ordering: Ordering[A]): IndexedSeq[Int] = {
    val result = new Array[Int](seq.length)

    seq.indices.foldLeft[List[Int]](Nil) { (stack, idx) =>
      val uStack = stack.dropWhile(i => ordering.lt(seq(i), seq(idx)))
      result(idx) = uStack.headOption.getOrElse(-1)
      idx :: uStack
    }

    result
  }

  /**
   * Monotonic Stack, right view
   *
   * @param seq elements, randomly accessible
   * @param ordering implicit ordering for gt operator
   * @tparam A type of each element
   * @return left view
   */
  def rightView[A](seq: IndexedSeq[A])(implicit ordering: Ordering[A]): IndexedSeq[Int] = {
    val result = new Array[Int](seq.length)

    seq.indices.foldRight[List[Int]](Nil) { (idx, stack) =>
      val uStack = stack.dropWhile(i => ordering.lt(seq(i), seq(idx)))
      result(idx) = uStack.headOption.getOrElse(-1)
      idx :: uStack
    }

    result
  }

  def largestRectangleInAHistogram(seq: Seq[Int]): Int = {

    @scala.annotation.tailrec
    def calculateView(idx: Int, increasingIndexes: List[Int] = Nil, resultList: List[Int] = Nil)(lastIndex: Int, nextIndex: Int => Int): List[Int] = {
      if (0 > idx || idx >= seq.length) resultList
      else {
        val remainingIndexes = increasingIndexes.dropWhile(i => seq(i) >= seq(idx))
        val result = remainingIndexes.headOption.map(i => math.abs(idx - i) - 1).getOrElse(math.abs(lastIndex - idx))

        calculateView(nextIndex(idx), idx :: remainingIndexes, result :: resultList)(lastIndex, nextIndex)
      }
    }

    val leftView = calculateView(0)(0, _ + 1).reverse
    val rightView = calculateView(seq.length - 1)(seq.length - 1, _ - 1)
    leftView.zip(rightView).zipWithIndex.map { case ((l, r), i) => (l + r + 1) * seq(i) }.max
  }
}
