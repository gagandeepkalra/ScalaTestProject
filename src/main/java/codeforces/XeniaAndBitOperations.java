package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/*

https://codeforces.com/contest/339/problem/D

Iterative Segment Trees- tried

 */
public class XeniaAndBitOperations {

    private static int[] segmentArr;

    private static boolean preCompute(int l, int r, int i) {
        if (l == r) return true;
        else {
            int m = (l + r) / 2;

            boolean left = preCompute(l, m, 2 * i);
            boolean right = preCompute(m + 1, r, 2 * i + 1);

            if (left & right) segmentArr[i] = segmentArr[2 * i] | segmentArr[2 * i + 1]; // OR
            else segmentArr[i] = segmentArr[2 * i] ^ segmentArr[2 * i + 1]; // XOR

            return !(left & right);
        }
    }

    private static void update(int i, boolean flag) {
        if (flag)
            segmentArr[i] = segmentArr[2 * i] | segmentArr[2 * i + 1];
        else
            segmentArr[i] = segmentArr[2 * i] ^ segmentArr[2 * i + 1];
        if (i > 0) update(i >> 1, !flag);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());
        int m = Integer.parseInt(st.nextToken());

        n = 1 << n; // 2^n
        segmentArr = new int[2 * n];

        st = new StringTokenizer(br.readLine());
        for (int i = n; i < 2 * n; ++i) segmentArr[i] = Integer.parseInt(st.nextToken());

        preCompute(0, n - 1, 1);

        while (m-- > 0) {

            st = new StringTokenizer(br.readLine());
            int p = Integer.parseInt(st.nextToken()) - 1, b = Integer.parseInt(st.nextToken());

            segmentArr[n + p] = b;
            update((n + p) >> 1, true);
            System.out.println(segmentArr[1]);
        }
    }
}
