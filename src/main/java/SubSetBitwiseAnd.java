import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;
import java.util.stream.IntStream;


/**
 *
 * AND Sum
 * Given an array of N numbers, you have to report the Sum of bitwise AND of all possible subsets of this array.
 * As answer can be large, report it after taking mod with 109+7.

 * Input:
 * First line contains a number T denoting the number of test cases.
 *
 * First line of each test case contains a number N denoting the number of elements in the array.
 * Second line contains the N elements of the array.
 *
 * Output:
 * For each test case output a single number denoting the Sum of bitwise AND of all possible subsets of the given array.
 *
 * Input Constraints:
 *
 * 1<=T<=10
 * 1<=N<=105
 * 1<=a[i]<=109
 *
 */
public class SubSetBitwiseAnd {

    final static int MOD = 1000000007;

    static long power(int a, int exp) {
        if (exp == 0) return 1;
        else if (exp % 2 == 0) {
            long result = power(a, exp / 2);
            return (result * result) % MOD;
        } else {
            return (power(a, exp - 1) * a) % MOD;
        }
    }

    public static void main(String[] srr) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int t = Integer.parseInt(st.nextToken());

        while (t-- > 0) {
            st = new StringTokenizer(br.readLine());
            int n = Integer.parseInt(st.nextToken());

            int[] arr = new int[n];
            int[] count = new int[32];

            st = new StringTokenizer(br.readLine());
            for (int i = 0; i < n; i++) {
                arr[i] = Integer.parseInt(st.nextToken());
                int x = arr[i];
                IntStream.range(0, 32).filter(j -> ((x >> j) & 1) == 1).forEach(j -> count[j]++);
            }

            long result = 0;

            for (int i = 0; i < 32; i++) {
                result = (result + ((power(2, count[i]) - 1) * power(2, i)) % MOD) % MOD;
            }

            System.out.println(result);

        }
    }
}
