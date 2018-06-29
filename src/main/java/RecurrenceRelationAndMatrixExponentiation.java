import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 *
 * The Travelling Ant
 * There is an Ant that lives in Baskerville and loves to travel. As Baskerville is a small place, it consists of only
 * 5 cities placed one next to each other.
 * There is a train between each successive cities ie between City 1 - City 2, City 2 - City 3, ... City 5 - City 1.
 * Note that our Ant loves to travel and gets happy after making exactly N train trips and returning back to home. Ant
 * lives in the city 1 from where she begins her journey. She asks you to find the number of ways she can make N train trips and come back to home.
 * Since the number of ways can be huge, print that number modulo 10^9 + 7.
 * Input
 * First line contains T, the number of test cases.
 * Then T lines follow.
 * Each line contains a single integer n, representing the number of train trips the ant needs to make.
 *
 * Output
 * For each test case, print a single line containing the answer to the problem.
 * Constraints
 * 1 <= T <= 1000
 * 0 <= n <= 10^18
 *
 */
public class RecurrenceRelationAndMatrixExponentiation {

    static long MOD = 1000000007; // 10^9 +7

    static long[][] multiply(long[][] a, long[][] b) {
        long[][] c = new long[4][4];
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                for (int k = 0; k < 4; k++) {
                    c[i][j] = (c[i][j] + (a[i][k] * b[k][j]) % MOD) % MOD;
                }
            }
        }
        return c;
    }

    final static long[][] a = {
            {0, 2, 0, 2},
            {1, 0, 0, 0},
            {0, 1, 1, 1},
            {0, 0, 1, 0}};

    static long[][] matrixExponentiation(long power) {
        if (power == 1) return a;

        if (power % 2 == 0) {
            long[][] result = matrixExponentiation(power / 2);
            return multiply(result, result);
        } else {
            return multiply(a, matrixExponentiation(power - 1));
        }
    }

    static long calculate(long n) {
        long a0 = 1, a1 = 0, a2 = 2;

        if (n == 0) return a0;
        else if (n == 1) return a1;
        else if (n == 2) return a2;

        long power = n - 2;

        long[][] aPower = matrixExponentiation(power);

        return (aPower[0][0] * 2 + aPower[0][2]) % MOD;
    }

    public static void main(String[] arr) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int t = Integer.parseInt(st.nextToken());
        while (t-- > 0) {
            st = new StringTokenizer(br.readLine());
            long n = Long.parseLong(st.nextToken());

            System.out.println(calculate(n));
        }

    }
}
