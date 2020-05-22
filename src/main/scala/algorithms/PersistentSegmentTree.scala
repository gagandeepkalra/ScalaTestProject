package algorithms

sealed abstract class Tree[+A] {
  def isEmpty: Boolean

  /**
   * unsafe downcast
   */
  def asNode[B >: A]: Node[B]
}

case object Leaf extends Tree[Nothing] {
  def isEmpty = true

  override def asNode[B >: Nothing]: Node[B] = throw new IllegalAccessError("not a Node!")
}

case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
  def isEmpty: Boolean = false

  def map[B](f: A => B): Node[B] = Node[B](f(value), Leaf, Leaf)

  override def asNode[B >: A]: Node[B] = this
}

case class PersistentSegmentTree[A] private(root: Node[A], l: Int, r: Int, combine: (A, A) => A) {
  def query(start: Int, endIncl: Int): A = {
    def query(root: Node[A], sR: Int, eR: Int, sD: Int, eD: Int): A = {
      if (sR == sD && eR == eD) root.value
      else {
        val m = (sR + eR) / 2

        val lT = root.left.asNode[A]
        val rT = root.right.asNode[A]

        if (eD <= m)
          query(lT, sR, m, sD, eD)
        else if (m < sD)
          query(rT, m + 1, eR, sD, eD)
        else
          combine(query(lT, sR, m, sD, m), query(rT, m + 1, eR, m + 1, eD))
      }
    }

    query(root, l, r, start, endIncl)
  }

  def update(i: Int, g: A => A): PersistentSegmentTree[A] = {
    def update(root: Node[A], sR: Int, eR: Int): Node[A] = {
      if (sR == eR) root.map(g)
      else {
        val m = (sR + eR) / 2

        val lT = root.left.asNode[A]
        val rT = root.right.asNode[A]

        val (uL, uR) = if (i <= m) (update(lT, sR, m), rT) else (lT, update(rT, m + 1, eR))

        Node(combine(uL.value, uR.value), uL, uR)
      }
    }

    this.copy(root = update(root, l, r))
  }
}

object PersistentSegmentTree {

  def apply[A](elements: IndexedSeq[A], f: (A, A) => A): PersistentSegmentTree[A] =
    new PersistentSegmentTree(makeRoot(end = elements.length - 1, atIndex = elements, combine = f), 0, elements.length - 1, f)

  private def makeRoot[A](start: Int = 0, end: Int, atIndex: Int => A, combine: (A, A) => A): Node[A] = {
    def makeRoot(l: Int, r: Int): Node[A] = {
      if (l == r) Node(atIndex(l), Leaf, Leaf)
      else {
        val m = (l + r) / 2
        val left = makeRoot(l, m)
        val right = makeRoot(m + 1, r)
        Node(combine(left.value, right.value), left, right)
      }
    }

    makeRoot(start, end)
  }
}


