package algorithms.range

private sealed trait Tree[+A] {
  def isEmpty: Boolean
}

private case object Leaf extends Tree[Nothing] {
  def isEmpty = true
}

private case class Node[A](value: A, left: Tree[A], right: Tree[A], update: Option[A => A] = None) extends Tree[A] {
  def isEmpty: Boolean = false

  def map[B](f: A => B): Node[B] = Node[B](f(value), Leaf, Leaf)
}

/**
  * Immutable Segment Tree, with support for lazy propagation for range updates
  */
case class PersistentLazySegmentTree[A] private (root: Node[A], l: Int, r: Int, combine: (A, A) => A) {

  def query(start: Int, endIncl: Int): (A, PersistentLazySegmentTree[A]) = {
    val (result, updated) = query(root, l, r, start, endIncl)

    (result, this.copy(root = updated))
  }

  def query(i: Int): (A, PersistentLazySegmentTree[A]) =
    query(i, i)

  def update(start: Int, endIncl: Int, g: A => A): PersistentLazySegmentTree[A] =
    this.copy(root = update(root, l, r, start, endIncl, g))

  def update(i: Int, g: A => A): PersistentLazySegmentTree[A] =
    update(i, i, g)

  private def pushUpdateDown(node: Node[A]): Node[A] = {
    node.update.fold(node) { g =>
      val leftUpdated = node.left match {
        case Leaf                          => Leaf
        case n @ Node(value, _, _, update) => n.copy(value = g(value), update = update.map(g compose _).orElse(Some(g)))
      }

      val rightUpdated = node.right match {
        case Leaf                          => Leaf
        case n @ Node(value, _, _, update) => n.copy(value = g(value), update = update.map(g compose _).orElse(Some(g)))
      }

      node.copy(left = leftUpdated, right = rightUpdated, update = None)
    }
  }

  private def query(node: Node[A], sR: Int, eR: Int, sD: Int, eD: Int): (A, Node[A]) = {
    if (sR == sD && eR == eD) (node.value, node)
    else {
      val updatedNode = pushUpdateDown(node)

      val m = (sR + eR) / 2

      val lT = updatedNode.left.asInstanceOf[Node[A]]
      val rT = updatedNode.right.asInstanceOf[Node[A]]

      if (eD <= m) {
        val (lResult, uL) = query(lT, sR, m, sD, eD)

        (lResult, updatedNode.copy(left = uL))
      } else if (m < sD) {
        val (rResult, uR) = query(rT, m + 1, eR, sD, eD)

        (rResult, updatedNode.copy(right = uR))
      } else {
        val (lResult, uL) = query(lT, sR, m, sD, m)
        val (rResult, uR) = query(rT, m + 1, eR, m + 1, eD)

        (combine(lResult, rResult), updatedNode.copy(left = uL, right = uR))
      }
    }
  }

  private def update(node: Node[A], sR: Int, eR: Int, sD: Int, eD: Int, g: A => A): Node[A] =
    if (sR == sD && eR == eD)
      node.copy(value = g(node.value), update = node.update.map(g compose _).orElse(Some(g)))
    else {
      val updatedNode = pushUpdateDown(node)

      val m = (sR + eR) / 2

      val lT = updatedNode.left.asInstanceOf[Node[A]]
      val rT = updatedNode.right.asInstanceOf[Node[A]]

      if (eD <= m) {
        val uL = update(lT, sR, m, sD, eD, g)

        updatedNode.copy(value = combine(uL.value, rT.value), left = uL)
      } else if (m < sD) {
        val uR = update(rT, m + 1, eR, sD, eD, g)

        updatedNode.copy(value = combine(lT.value, uR.value), right = uR)
      } else {
        val uL = update(lT, sR, m, sD, m, g)
        val uR = update(rT, m + 1, eR, m + 1, eD, g)

        Node(value = combine(uL.value, uR.value), left = uL, right = uR)
      }
    }
}

object PersistentLazySegmentTree {

  def apply[A](elements: IndexedSeq[A], combine: (A, A) => A): PersistentLazySegmentTree[A] =
    new PersistentLazySegmentTree(
      root = makeRoot(start = 0, end = elements.length - 1, atIndex = elements, combine = combine),
      l = 0,
      r = elements.length - 1,
      combine = combine
    )

  def apply[A](start: Int, endIncl: Int, elements: Int => A, combine: (A, A) => A): PersistentLazySegmentTree[A] =
    new PersistentLazySegmentTree(
      root = makeRoot(start = start, end = endIncl, atIndex = elements, combine = combine),
      l = start,
      r = endIncl,
      combine = combine
    )

  private def makeRoot[A](start: Int, end: Int, atIndex: Int => A, combine: (A, A) => A): Node[A] = {
    def makeRoot(l: Int, r: Int): Node[A] = {
      if (l == r) Node(atIndex(l), Leaf, Leaf)
      else {
        val m     = (l + r) / 2
        val left  = makeRoot(l, m)
        val right = makeRoot(m + 1, r)
        Node(combine(left.value, right.value), left, right)
      }
    }

    makeRoot(start, end)
  }
}
