def query(l: Int, r: Int, i: Int): Int = {
  if (l == r) 0
  else math.min(query(l, (l + r) / 2, 2 * i + 1), query((l + r) / 2, r, 2 * i + 2))
}

val f1 = (query _).tupled
//f1.tupled