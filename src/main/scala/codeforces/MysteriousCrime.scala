package codeforces

/*

http://codeforces.com/contest/1043/problem/D

 */
object MysteriousCrime {

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val input = new Array[Array[Int]](m)

    val trackNext = Array.fill(n + 1)(0)

    (0 until m).foreach(i => {

      input(i) = io.StdIn.readLine.split(" ").map(_.toInt)

      (0 until n).foreach(j => {

        trackNext(input(i)(j)) = trackNext(input(i)(j)) match {

          case 0 => if (j + 1 < n) input(i)(j + 1) else -1 // uninitialized

          case x if j + 1 < n && x != input(i)(j + 1) || j + 1 == n && x != -1 => -1 // discarding now

          case y => y // all cool bro
        }
      })

    })


    def computeResult(ls: List[Int], counter: Int, res: Long): Long = {

      ls match {

        case Nil => res

        case h :: t if trackNext(h) == -1 => computeResult(t, 0, res)

        case h :: t if trackNext(h) != -1 => computeResult(t, counter + 1, res + counter + 1)

      }

    }


    println(n + computeResult(input(0).toList, 0, 0))
  }

}
