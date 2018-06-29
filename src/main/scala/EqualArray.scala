/*

Equal Array
You are given an array  of size

Find the minimum non negative number x such that there exist an index j and when you can replace Aj by Aj + x, the sum
of elements of array from index 1 to j  and j+1 to n  becomes equal, where . Assume array to be 1-indexed.

If there is no possible x print -1 in separate line.

Input Format

The first line contains t, the number of test cases.
For each Test case :
The first line contains an integer n, size of the array.
The second line contains n space-separated integers, the  of which is .

Sum of N all over testcases doesn't not exceed n.

Output Format

For each test case, print the required answer in separate line.

Sample Input
1
5
1 2 3 2 1
Sample Output
3
 */

object EqualArray {


  def solve(t: Int): Unit = {
    val n = io.StdIn.readInt()

    val arr = io.StdIn.readLine.split(" ").map(_.toLong)
    var total: Long = arr.sum

    var result = Long.MaxValue
    var leftsum: Long = 0

    var i = 0
    while (i <= n - 2) {
      leftsum += arr(i)

      if (leftsum <= total - leftsum) {
        result = math.min(result, total - 2 * leftsum)
      }

      i += 1
    }

    System.out.println(if (result == Long.MaxValue) -1 else result)

  }

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(solve)
  }
}
