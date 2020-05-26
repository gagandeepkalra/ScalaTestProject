package algorithms

import scala.reflect.ClassTag

class SegmentTree[A] private(segmentArr: Array[A], l: Int, r: Int, combine: (A, A) => A) {

  def update(idx: Int, g: A => A): Unit = {
    def update(l: Int, r: Int, i: Int): Unit = {
      if (l == r) segmentArr(i) = g(segmentArr(i))
      else {
        val m = (l + r) / 2
        if (idx <= m) update(l, m, 2 * i + 1) else update(m + 1, r, 2 * i + 2)
        segmentArr(i) = combine(segmentArr(2 * i + 1), segmentArr(2 * i + 2))
      }
    }

    update(l, r, 0)
  }

  def query(left: Int, right: Int): A = {
    def query(l: Int, r: Int, i: Int, x: Int, y: Int): A = {
      if (l == x && r == y) segmentArr(i)
      else {
        val m = (l + r) / 2

        if (y <= m) query(l, m, 2 * i + 1, x, y)
        else if (m < x) query(m + 1, r, 2 * i + 2, x, y)
        else {
          combine(query(l, m, 2 * i + 1, x, m), query(m + 1, r, 2 * i + 2, m + 1, y))
        }
      }
    }

    query(l, r, 0, left, right)
  }
}

object SegmentTree {

  def apply[A: ClassTag](l: Int, r: Int, combine: (A, A) => A): SegmentTree[A] = {
    val segmentArr = new Array[A](4 * (r - l + 1))
    new SegmentTree(segmentArr, l, r, combine)
  }

  def apply[A: ClassTag](elements: IndexedSeq[A], combine: (A, A) => A): SegmentTree[A] = {
    val segmentArr = new Array[A](4 * elements.length)

    def preCompute(l: Int, r: Int, i: Int): Unit = {
      if (l == r) segmentArr(i) = elements(l)
      else {
        val m = (l + r) / 2
        preCompute(l, m, 2 * i + 1)
        preCompute(m + 1, r, 2 * i + 2)
        segmentArr(i) = combine(segmentArr(2 * i + 1), segmentArr(2 * i + 2))
      }
    }

    preCompute(0, elements.length - 1, 0)
    new SegmentTree(segmentArr, 0, elements.length - 1, combine)
  }
}
