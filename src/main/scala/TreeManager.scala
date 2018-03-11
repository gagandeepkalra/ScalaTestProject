// General Trees in Scala, with capability to traverse any direction, left child - right sibling implementation
object TreeManager {

  sealed trait Tree

  case class Node(var leftChild: Tree, var value: Int,
                  var leftSibling: Tree, var rightSibling: Tree,
                  var parent: Tree) extends Tree

  case object EmptyNode extends Tree


  def printValue(tree: Tree) = tree match {
    case node: Node => println(node.value)
    case _ =>
  }

  def changeValue(tree: Tree, value: Int) = tree match {
    case node: Node => node.value = value
    case _ =>
  }

  def visitChild(tree: Tree, n: Int): Tree = {
    var child = tree.asInstanceOf[Node].leftChild
    for (_ <- 1 until n) child = child.asInstanceOf[Node].rightSibling

    child
  }

  def insertLeftSibling(tree: Tree, x: Int): Unit = {
    val current = tree.asInstanceOf[Node]
    val newNode = Node(EmptyNode, x, current.leftSibling, current, current.parent)

    current.leftSibling match {
      case node: Node => node.rightSibling = newNode
      case _ => {
        current.parent.asInstanceOf[Node].leftChild = newNode
      }
    }
    current.leftSibling = newNode
  }

  def insertRightSibling(tree: Tree, x: Int): Unit = {
    val current = tree.asInstanceOf[Node]
    val newNode = Node(EmptyNode, x, current, current.rightSibling, current.parent)

    current.rightSibling match {
      case node: Node => node.leftSibling = newNode
      case _ =>
    }
    current.rightSibling = newNode
  }

  def insertChild(tree: Tree, x: Int): Unit = {
    val current = tree.asInstanceOf[Node]
    val newNode = Node(EmptyNode, x, EmptyNode, current.leftChild, current)

    current.leftChild match {
      case node: Node => node.leftSibling = newNode
      case _ =>
    }
    current.leftChild = newNode
  }

  def deleteSubtree(tree: Tree): Tree = {
    tree.asInstanceOf[Node].leftSibling match {
      case node:Node => node.rightSibling = tree.asInstanceOf[Node].rightSibling
      case _ => tree.asInstanceOf[Node].parent.asInstanceOf[Node].leftChild = tree.asInstanceOf[Node].rightSibling
    }

    tree.asInstanceOf[Node].rightSibling match {
      case node:Node => node.leftSibling = tree.asInstanceOf[Node].leftSibling
      case _ =>
    }

    tree.asInstanceOf[Node].parent
  }

  def main(args: Array[String]): Unit = {
    var tree: Tree = Node(EmptyNode, 0, EmptyNode, EmptyNode, EmptyNode)

    (1 to io.StdIn.readInt()).foreach(f = _ => {
      io.StdIn.readLine.split(" ").toList match {
        case "change" :: x :: Nil => changeValue(tree, x.toInt)
        case "print" :: Nil => printValue(tree)
        case "visit" :: "left" :: Nil => tree = tree.asInstanceOf[Node].leftSibling
        case "visit" :: "right" :: Nil => tree = tree.asInstanceOf[Node].rightSibling
        case "visit" :: "parent" :: Nil => tree = tree.asInstanceOf[Node].parent
        case "visit" :: "child" :: n :: Nil => tree = visitChild(tree, n.toInt)
        case "insert" :: "left" :: n :: Nil => insertLeftSibling(tree, n.toInt)
        case "insert" :: "right" :: n :: Nil => insertRightSibling(tree, n.toInt)
        case "insert" :: "child" :: n :: Nil => insertChild(tree, n.toInt)
        case "delete" :: Nil => tree = deleteSubtree(tree)
        case _ => null
      }
    })
  }

}
