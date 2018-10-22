package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;

/*

E. Optimize! :=> https://codeforces.com/contest/338/problem/E

Segment Trees, Lazy, Binary Search

 */
public class Optimize {

    private static void preCompute(int[] segmentArr, int len, int l, int r, int i) { // 1 based segmentArr
        if (l == r) segmentArr[i] = len - l;
        else {
            int m = (l + r) >>> 1;
            preCompute(segmentArr, len, l, m, i << 1);
            preCompute(segmentArr, len, m + 1, r, (i << 1) | 1);

            segmentArr[i] = Math.min(segmentArr[i << 1], segmentArr[(i << 1) | 1]);
        }
    }

    private static void updateWithAmount(int[] segmentArr, int[] lazy, int findl, int findr, int amount, int l, int r, int i) {
        if (r < findl || findr < l) {
            // out of range
        } else {

            if (findl <= l && r <= findr) {
                segmentArr[i] += amount;
                lazy[i] += amount;

                return; // don't go any further boy!
            }

            if (lazy[i] != 0) { // lazy set => children needs to be updated
                lazy[i << 1] += lazy[i];
                segmentArr[i << 1] += lazy[i];

                lazy[(i << 1) | 1] += lazy[i];
                segmentArr[(i << 1) | 1] += lazy[i];

                lazy[i] = 0;
            }

            int m = (l + r) >>> 1;
            updateWithAmount(segmentArr, lazy, findl, findr, amount, m + 1, r, (i << 1) | 1);
            updateWithAmount(segmentArr, lazy, findl, findr, amount, l, m, i << 1);

            segmentArr[i] = Math.min(segmentArr[i << 1], segmentArr[(i << 1) | 1]);
        }
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken()), len = Integer.parseInt(st.nextToken()), h = Integer.parseInt(st.nextToken());

        int[] a = new int[n], b = new int[len];

        st = new StringTokenizer(br.readLine());
        for (int i = 0; i < len; i++) b[i] = Integer.parseInt(st.nextToken());
        Arrays.sort(b);

        st = new StringTokenizer(br.readLine());
        for (int i = 0; i < n; i++) a[i] = Integer.parseInt(st.nextToken());

        int[] segmentArr = new int[4 * len], lazy = new int[4 * len];
        preCompute(segmentArr, len, 0, len - 1, 1); // len, len-1, len-2 ....

        int result = 0;
        int[] index = new int[n]; // save indexes of elements used in array b for each element fo a

        for (int i = 0, negativeOnesCount = 0; i < n; i++) {
            int key = h - a[i]; // now find index of element >= key in array b

            index[i] = -1;
            if (key <= b[len - 1]) {
                int l = 0, r = len - 1;
                while (l < r) { // 5 7
                    int mid = (l + r) >>> 1;
                    if (key <= b[mid])
                        r = mid;
                    else // key > b[mid]
                        l = mid + 1;
                }
                index[i] = r;
            }

            if (index[i] != -1)
                updateWithAmount(segmentArr, lazy, 0, index[i], -1, 0, len - 1, 1); // increase 0 to r
            else
                negativeOnesCount++;

            if (i >= len)
                if (index[i - len] != -1)
                    updateWithAmount(segmentArr, lazy, 0, index[i - len], 1, 0, len - 1, 1); // revert
                else
                    negativeOnesCount--;


            if (i >= len - 1 && negativeOnesCount == 0 && segmentArr[1] >= 0)
                result++;
        }

        System.out.println(result);
    }
}
