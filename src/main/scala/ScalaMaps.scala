import java.util.Scanner

object ScalaMaps {

  // repeating atleast k times
  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    val n = sc.nextInt

    (1 to n).foreach(_ => {
      val n = sc.nextInt
      val k = sc.nextInt

      sc.nextLine
      val arr = sc.nextLine().split(" ").filter(_ != "").map(_.toInt).toList

      val map = collection.mutable.Map[Int, Int]()

      arr.foreach(i => {
        map.put(i, map.getOrElse(i, 0) + 1)
      })

      if (arr.count(i => {
        if (map.getOrElse(i, -1) >= k) {
          map.remove(i)
          print(i + " ")
          true
        } else false
      }) == 0) print(-1)

      println
    })
  }
}
