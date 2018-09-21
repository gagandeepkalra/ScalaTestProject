package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

/*

https://codeforces.com/contest/61/problem/E

Inversions for each element.

 */
public class EnemyIsWeak {
    private static int[] segmentArr; // 0 is not used, 1 to n-1 are internal nodes, n to 2n-1 are leaves

    private static void preCompute() {
        int n = segmentArr.length / 2;
        for (int i = n - 1; i >= 1; i--) {
            segmentArr[i] = segmentArr[i << 1] + segmentArr[(i << 1) + 1];
        }
    }

    private static void update(int idx) {
        int n = segmentArr.length / 2;
        for (segmentArr[idx += n]--; idx > 1; idx >>= 1) segmentArr[idx >> 1] = segmentArr[idx] + segmentArr[idx ^ 1];
    }

    private static int query(int l, int r) { // even => left child, odd => right child, [l, r)
        int res = 0, n = segmentArr.length / 2;
        for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
            if ((l & 1) == 1) res += segmentArr[l++];
            if ((r & 1) == 1) res += segmentArr[--r];
        }
        return res;
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());

        int[] input = new int[n], rangedInput = new int[n];
        st = new StringTokenizer(br.readLine());

        Map<Integer, Integer> indexOf = new HashMap<>();
        for (int i = 0; i < n; i++) {
            input[i] = Integer.parseInt(st.nextToken());
            indexOf.put(input[i], i);
        }

        Arrays.sort(input);
        for (int i = 0; i < n; i++) rangedInput[indexOf.get(input[i])] = i;

        int[] left = new int[n], right = new int[n];
        computeLeftAndRightView(left, right, rangedInput);

        long result = 0;
        for (int i = 0; i < n; i++) {
            result += (long) left[i] * (long) right[i];
        }
        System.out.println(result);
    }

    private static void computeLeftAndRightView(int[] left, int[] right, int[] input) {
        int n = input.length;
        segmentArr = new int[2 * n];

        Arrays.fill(segmentArr, n, 2 * n, 1);
        preCompute();
        for (int i = 0; i < n; update(input[i++])) right[i] = query(0, input[i]);

        Arrays.fill(segmentArr, n, 2 * n, 1);
        preCompute();
        for (int i = n - 1; i >= 0; update(input[i--])) left[i] = query(input[i] + 1, n);
    }

}
