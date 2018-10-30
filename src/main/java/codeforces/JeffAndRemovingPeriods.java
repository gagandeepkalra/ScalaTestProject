package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

/*

D. Jeff and Removing Periods :=> https://codeforces.com/contest/351/problem/D
Unique element count in a range, arithmetic progression

 */
public class JeffAndRemovingPeriods {

    private static class Query {
        int l, r, result = 0;

        public Query(int l, int r) {
            this.l = l;
            this.r = r;
        }
    }

    private static int[] segmentSum, segmentMin, diff;
    private static int prevDiff = 0, prevMin = 0, parent, now;

    private static void update(int i, int l, int r, int idx, int value) { // 1 based
        if (l == r) {
            segmentSum[i] += value;
            if (value == -1) { // reset previous repeated sum addition
                prevDiff = diff[i];
                prevMin = segmentMin[i];
                segmentMin[i] = Integer.MAX_VALUE; // reset previous repeated values
            } else {
                if (parent == 0) diff[i] = 0;
                else diff[i] = now - parent;

                if (prevDiff == 0 || prevDiff == now - parent) segmentMin[i] = prevMin;
                else segmentMin[i] = parent - prevDiff + 1;
            }

        } else {
            int m = (l + r) >>> 1;
            if (idx <= m) update(i << 1, l, m, idx, value);
            else update(i << 1 | 1, m + 1, r, idx, value);

            segmentSum[i] = segmentSum[i << 1] + segmentSum[i << 1 | 1];
            segmentMin[i] = Math.min(segmentMin[i << 1], segmentMin[i << 1 | 1]);
        }
    }

    private static int findSum(int i, int l, int r, int idx) { // [idx, n]
        if (l == r) {
            return segmentSum[i];
        } else {
            int m = (l + r) >>> 1;
            if (idx <= m) return findSum(i << 1, l, m, idx) + segmentSum[i << 1 | 1];
            else return findSum(i << 1 | 1, m + 1, r, idx);
        }
    }

    private static int findMin(int i, int l, int r, int idx) { // [idx, n]
        if (l == r) {
            return segmentMin[i];
        } else {
            int m = (l + r) >>> 1;
            if (idx <= m) return Math.min(findMin(i << 1, l, m, idx), segmentMin[i << 1 | 1]);
            else return findMin(i << 1 | 1, m + 1, r, idx);
        }
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(bf.readLine());

        int n = Integer.parseInt(st.nextToken());
        int[] arr = new int[n + 1];
        ArrayList<Integer>[] rBuckets = new ArrayList[n + 1];

        st = new StringTokenizer(bf.readLine());
        for (int i = 1; i <= n; i++) {
            arr[i] = Integer.parseInt(st.nextToken());
            rBuckets[i] = new ArrayList<Integer>();
        }

        st = new StringTokenizer(bf.readLine());
        int q = Integer.parseInt(st.nextToken());

        Query[] queries = new Query[q];
        for (int i = 0; i < q; i++) {
            st = new StringTokenizer(bf.readLine());
            int l = Integer.parseInt(st.nextToken()), r = Integer.parseInt(st.nextToken());
            queries[i] = new Query(l, r);
            rBuckets[r].add(i);
        }

        int[] lastIdx = new int[100001];
        segmentSum = new int[4 * n];
        segmentMin = new int[4 * n];
        Arrays.fill(segmentMin, Integer.MAX_VALUE);
        diff = new int[4 * n];

        for (int i = 1; i <= n; i++) {
            parent = lastIdx[arr[i]];
            now = i;
            prevDiff = 0;
            prevMin = 0;

            if (parent == 0) {
                update(1, 1, n, i, 1);
            } else {
                update(1, 1, n, parent, -1); // reset previous occurrence of arr[i]
                update(1, 1, n, i, 1); // push 1 to latest seen
            }

            lastIdx[arr[i]] = i;

            for (int j : rBuckets[i]) { // for queries with r == i
                queries[j].result = findSum(1, 1, n, queries[j].l);
                if (findMin(1, 1, n, queries[j].l) > queries[j].l) queries[j].result++; // problem child
            }
        }

        for (int i = 0; i < q; i++) {
            System.out.println(queries[i].result);
        }
    }
}
