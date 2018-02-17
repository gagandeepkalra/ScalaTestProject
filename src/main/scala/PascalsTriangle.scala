import scala.collection.mutable.ListBuffer

object PascalsTriangle {

  def printPascalsTriangle(k: Int): List[Int] = {
    if (k == 1) {
      println("1")
      List(1)
    }
    else {
      val list: List[Int] = printPascalsTriangle(k - 1)

      val newListBuffer = ListBuffer[Int](1)

      (1 until list.size).foreach { i: Int =>
        newListBuffer += list(i) + list(i - 1)
      }
      newListBuffer += 1
      newListBuffer.toList

      val newList = newListBuffer.toList

      for (i <- newList.indices) {
        print(newList(i) + " ")
      }
      println
      newList
    }
  }

  def main(args: Array[String]): Unit = {
    printPascalsTriangle(io.StdIn.readInt())
  }
}