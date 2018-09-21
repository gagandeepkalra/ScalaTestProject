package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

/*

https://codeforces.com/contest/459/problem/D

Iterative Segment Trees- Range Sum Query

 */
public class PashmakandParmidasProblem {

    private static int[] segmentArr; // 0 is not used, 1 to n-1 are internal nodes, n to 2n-1 are leaves

    private static int[] auxx;

    private static void preCompute() {
        int n = auxx.length;
        segmentArr = new int[2 * n];

        System.arraycopy(auxx, 0, segmentArr, n, n);
        for (int i = n - 1; i >= 1; i--) {
            segmentArr[i] = segmentArr[i << 1] + segmentArr[(i << 1) + 1];
        }
    }

    private static void update(int idx) {
        int n = auxx.length;
        for (segmentArr[idx += n]--; idx > 1; idx >>= 1) segmentArr[idx >> 1] = segmentArr[idx] + segmentArr[idx ^ 1];
    }

    private static int query(int l, int r) { // even => left child, odd => right child, [l, r)
        int res = 0, n = auxx.length;
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

        int[] input = new int[n];
        st = new StringTokenizer(br.readLine());
        for (int i = 0; i < n; i++) input[i] = Integer.parseInt(st.nextToken());

        int[] left = new int[n], right = new int[n];
        computeLeftAndRightView(left, right, input);

        auxx = new int[n + 1]; // max frequency be n, when all same, min be 1 [0, n]
        for (int i = 0; i < n; i++) auxx[right[i]]++;

        preCompute();

        long result = 0;
        for (int i = 0; i < n; i++) {
            update(right[i]); // decrement frequency of right[i] from segment
            result += query(0, left[i]); // find count of elements less than left[i]
        }
        System.out.println(result);
    }

    private static void computeLeftAndRightView(int[] left, int[] right, int[] input) {
        Map<Integer, Integer> frequency = new HashMap<>();

        for (int i = 0; i < input.length; i++) {
            int count = frequency.getOrDefault(input[i], 0) + 1;
            frequency.put(input[i], count);
            left[i] = count;
        }

        frequency.clear();
        for (int i = input.length - 1; i >= 0; i--) {
            int count = frequency.getOrDefault(input[i], 0) + 1;
            frequency.put(input[i], count);
            right[i] = count;
        }
    }
}
