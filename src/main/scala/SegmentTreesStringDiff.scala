/**
  * lexicographically compare two strings (0's and 1's) (Q update queries on second String)
  */
object SegmentTreesStringDiff {
  def main(args: Array[String]): Unit = {

    val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = io.StdIn.readLine
    val B = io.StdIn.readLine.toCharArray

    val segmentArr = new Array[Int](heightOfSegmentTree(n)) // (l, r) represents index of differing element, else -1

    def preCompute(l: Int, r: Int, i: Int): Unit = {
      if (l == r) {
        segmentArr(i) = if (B(l) == A.charAt(l)) -1 else l
      }
      else {
        val m = (l + r) / 2

        preCompute(l, m, 2 * i + 1)
        preCompute(m + 1, r, 2 * i + 2)
        segmentArr(i) = if (segmentArr(2 * i + 1) != -1) segmentArr(2 * i + 1) else segmentArr(2 * i + 2)
      }
    }

    def update(l: Int, r: Int, i: Int)(implicit index: Int): Unit = {
      if (l == r) {
        B(l) = '1'
        segmentArr(i) = if (B(l) == A.charAt(l)) -1 else l
      }
      else {
        val m = (l + r) / 2

        if (index <= m) update(l, m, 2 * i + 1) else update(m + 1, r, 2 * i + 2)

        segmentArr(i) = if (segmentArr(2 * i + 1) != -1) segmentArr(2 * i + 1) else segmentArr(2 * i + 2)
      }
    }

    preCompute(0, n - 1, 0)

    for (_ <- 1 to q) {
      val index = io.StdIn.readInt

      update(0, n - 1, 0)(index - 1)
      val differingIndex = segmentArr(0)
      if (differingIndex != -1 && B(differingIndex) < A.charAt(differingIndex)) println("NO") else println("YES")
    }

  }


  def heightOfSegmentTree(n: Int): Int = {
    2 * Math.pow(2, Math.ceil(Math.log(n) / Math.log(2)).toInt).toInt - 1
  }
}

// In Java

/*

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

public class SegmentTreesStringDiffJavaVersion {
    static double heightOfSegmentTree(int n) {
        return 2 * Math.pow(2, Math.ceil(Math.log(n) / Math.log(2))) - 1;
    }

    static String A;
    static char[] B;
    static int[] segmentArr;

    static void preCompute(int l, int r, int i) {
        if (l > r) {
        } else if (l == r) {
            if (B[l] == A.charAt(l)) segmentArr[i] = -1;
            else segmentArr[i] = l;
        } else {
            int m = (l + r) / 2;

            preCompute(l, m, 2 * i + 1);
            preCompute(m + 1, r, 2 * i + 2);
            if (segmentArr[2 * i + 1] != -1) segmentArr[i] = segmentArr[2 * i + 1];
            else segmentArr[i] = segmentArr[2 * i + 2];
        }
    }

    static void update(int l, int r, int i, int index) {
        if (l > r) {
        } else if (l == r) {
            B[l] = '1';
            if (B[l] == A.charAt(l)) segmentArr[i] = -1;
            else segmentArr[i] = l;
        } else {
            int m = (l + r) / 2;

            if (index <= m) update(l, m, 2 * i + 1, index);
            else update(m + 1, r, 2 * i + 2, index);

            if (segmentArr[2 * i + 1] != -1) segmentArr[i] = segmentArr[2 * i + 1];
            else segmentArr[i] = segmentArr[2 * i + 2];
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());
        int q = Integer.parseInt(st.nextToken());

        A = new StringTokenizer(br.readLine()).nextToken();
        B = new StringTokenizer(br.readLine()).nextToken().toCharArray();

        segmentArr = new int[(int) heightOfSegmentTree(n)]; // (l, r) represents index of differing element, else -1

        preCompute(0, n - 1, 0);

        while (q-- > 0) {

            st = new StringTokenizer(br.readLine());
            int index = Integer.parseInt(st.nextToken());

            update(0, n - 1, 0, index - 1);

            int differingIndex = segmentArr[0];
            if (differingIndex != -1 && B[differingIndex] < A.charAt(differingIndex)) System.out.println("NO");
            else System.out.println("YES");
        }
    }
}


 */
