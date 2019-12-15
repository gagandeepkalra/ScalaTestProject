package codeforces;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;
import java.util.stream.IntStream;

/*
/*
https://codeforces.com/contest/628/problem/D

Digit DP
 */
public class MagicNumbersJava {

    static int mod = 1000000000 + 7;

    static int m, d;
    static int[][][] dp;

    private static int solve(int[] digits) {
        int Idx = digits.length;

        dp[Idx][0][0] = dp[Idx][0][1] = 1;

        for (int idx = Idx - 1; idx >= 0; idx--) {
            for (int r = 0; r < m; r++) {
                if (idx % 2 == 1) {
                    dp[idx][r][0] = dp[idx + 1][(r * 10 + d) % m][0];
                    if (d < digits[idx])
                        dp[idx][r][1] = dp[idx + 1][(r * 10 + d) % m][0];
                    else if (d == digits[idx])
                        dp[idx][r][1] = dp[idx + 1][(r * 10 + d) % m][1];
                    else dp[idx][r][1] = 0;

                } else {
                    long temp = 0L;
                    for (int i = 0; i <= 9; i++) {
                        if (i == d) continue;
                        temp = (temp + dp[idx + 1][(r * 10 + i) % m][0]) % mod;
                    }
                    dp[idx][r][0] = (int) temp;

                    temp = 0;
                    for (int i = 0; i < digits[idx]; i++) {
                        if (i == d) continue;
                        temp = (temp + dp[idx + 1][(r * 10 + i) % m][0]) % mod;
                    }
                    if (digits[idx] != d)
                        temp = (temp + dp[idx + 1][(r * 10 + digits[idx]) % m][1]) % mod;

                    dp[idx][r][1] = (int) temp;
                }
            }
        }
        return dp[0][0][1];
    }

    private static int isGood(int[] digits) {
        if (IntStream.range(0, digits.length).mapToObj(i -> {
            if (i % 2 == 0) return digits[i] != d;
            else return digits[i] == d;
        }).reduce(true, (b1, b2) -> b1 && b2) && Arrays.stream(digits).reduce(0, (rem, c) -> (rem * 10 + c) % m) == 0)
            return 1;
        else return 0;

    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        m = Integer.parseInt(st.nextToken());
        d = Integer.parseInt(st.nextToken());
        int[] lDigits = br.readLine().chars().map(i -> i - '0').toArray(), rDigits = br.readLine().chars().map(i -> i - '0').toArray();

        int Idx = lDigits.length;

        dp = new int[Idx + 1][m][2];

        System.out.println((solve(rDigits) - solve(lDigits) + isGood(lDigits) + mod) % mod);

    }
}
