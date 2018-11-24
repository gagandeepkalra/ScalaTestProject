package codeforces

/*

http://codeforces.com/contest/1043/problem/C

 */
object SmallestWord {

  def main(args: Array[String]): Unit = {
    val input = io.StdIn.readLine

    for (i <- 0 to input.length - 2) {
      if (input(i) == input(i+1)) print("0 ")
      else print("1 ")
    }

    if(input.last == 'a') print("1") else print("0")
  }

}
