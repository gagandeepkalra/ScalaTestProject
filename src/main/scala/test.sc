case class pair(a: Long, h: Long) {
  def value(p: Int): Long = a + (p - 1) * h
}

val arr: Array[pair] = Array(pair(2, 5), pair(5, 40), pair(3, 10))

arr.sortBy(_.value(3))

arr
