object Trees {

  case class Node(left: Int, right: Int)

  def printInorder(tree: Array[Node], index: Int): Unit = {
    if (index < 1) return
    printInorder(tree, tree(index).left)
    print(index + " ")
    printInorder(tree, tree(index).right)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt
    val tree = new Array[Node](n + 1)
    (1 to n).foreach(i => io.StdIn.readLine.split(" ").map(_.toInt).grouped(2) foreach (a => tree(i) = Node(a.head, a.last)))

    (1 to io.StdIn.readInt).foreach(_ => {
      val d = io.StdIn.readInt

      def swapAtMultiplesOfDepth(index: Int, depth: Int): Unit = {
        if (index == -1) return

        if (depth % d == 0) tree(index) = Node(tree(index).right, tree(index).left)

        swapAtMultiplesOfDepth(tree(index).left, depth + 1)
        print(index + " ")
        swapAtMultiplesOfDepth(tree(index).right, depth + 1)
      }

      swapAtMultiplesOfDepth(1, 1)

//      (1 to n)
//        .filter(i => ((math.log(i) / math.log(2)).floor.toInt + 1) % d == 0)
//        .foreach(i => {
//          println(i)
//          tree(i) = Node(tree(i).right, tree(i).left)
//        })
//    printInorder(tree, 1)
      println
    })
  }
}
