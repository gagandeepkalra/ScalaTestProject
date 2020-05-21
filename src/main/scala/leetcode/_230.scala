package leetcode

/*
https://leetcode.com/problems/kth-smallest-element-in-a-bst/

[Tree]

kth smallest in a Binary search tree, we use scala [Either] to return remaining k value or the result
 */
object _230 {

  sealed trait Tree

  case class TreeNode(value: Int = 0, left: Tree, right: Tree) extends Tree

  object Empty extends Tree

  def kthSmallest(root: Tree, k: Int): Int = {
    recur(root, k).right.get
  }

  def recur(root: Tree, k: Int): Either[Int, Int] = { // left remainingK else right result
    root match {
      case Empty => Left(k)
      case TreeNode(value, left, right) =>
        recur(left, k).left
          .flatMap(kRemaining => if (kRemaining == 1) Right(value) else recur(right, kRemaining - 1))
    }
  }
}
