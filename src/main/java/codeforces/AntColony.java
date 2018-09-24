package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.function.BiFunction;

/*

https://codeforces.com/contest/474/problem/F

RMQ, Range GCD, occurrences in a given index range

 */
public class AntColony {

    private static int[] preCompute(int[] input, BiFunction<Integer, Integer, Integer> f) {
        int n = input.length;
        int[] segmentArr = new int[2 * n];
        System.arraycopy(input, 0, segmentArr, n, n);

        for (int i = n - 1; i > 0; i--) segmentArr[i] = f.apply(segmentArr[i << 1], segmentArr[(i << 1) + 1]);
        return segmentArr;
    }

    private static int query(int[] segmentArr, BiFunction<Integer, Integer, Integer> f, int l, int r) { // [l, r)
        int n = segmentArr.length / 2;

        int res = segmentArr[l + n]; // neutral element
        for (l += n, r += n; l < r; l >>= 1, r >>= 1) { // odd => right child, even => left child
            if ((l & 1) == 1) res = f.apply(res, segmentArr[l++]);
            if ((r & 1) == 1) res = f.apply(res, segmentArr[--r]);
        }
        return res;
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());

        Map<Integer, ArrayList<Integer>> occurrences = new HashMap<>();
        st = new StringTokenizer(br.readLine());
        int input[] = new int[n];
        for (int i = 0; i < n; i++) {
            input[i] = Integer.parseInt(st.nextToken());
            occurrences.putIfAbsent(input[i], new ArrayList<>());
            occurrences.get(input[i]).add(i);
        }

        int[] minSegmentArr = preCompute(input, Math::min);
        int[] gcdSegmentArr = preCompute(input, AntColony::gcd);

        st = new StringTokenizer(br.readLine());
        int m = Integer.parseInt(st.nextToken());


        while (m-- > 0) {
            st = new StringTokenizer(br.readLine());
            int l = Integer.parseInt(st.nextToken()), r = Integer.parseInt(st.nextToken());

            int min = query(minSegmentArr, Math::min, l - 1, r), gcd = query(gcdSegmentArr, AntColony::gcd, l - 1, r);

            if (min == gcd) {
                ArrayList<Integer> indexes = occurrences.get(gcd); // sorted list of occurrences, won't be nil
                System.out.println(r - l + 1 - countOccurrences(indexes, l - 1, r - 1));
            } else {
                System.out.println(r - l + 1);
            }
        }
    }

    private static int countOccurrences(ArrayList<Integer> indexes, int l, int r) { // inclusive 2 4
        int leftIdx = Collections.binarySearch(indexes, l), rightIdx = Collections.binarySearch(indexes, r);

        if (leftIdx >= 0 && rightIdx >= 0) {
            return rightIdx - leftIdx + 1;
        } else if (leftIdx < 0 && rightIdx < 0) {
            leftIdx = -leftIdx - 1;
            rightIdx = -rightIdx - 1;
            return rightIdx - leftIdx;
        } else if (leftIdx >= 0 && rightIdx < 0) {
            rightIdx = -rightIdx - 1;
            return rightIdx - leftIdx;
        } else {  //if (leftIdx < 0 && rightIdx >= 0) {
            leftIdx = -leftIdx - 1;
            return rightIdx - leftIdx + 1;
        }
    }

    private static int lowerBoundBinarySearch(ArrayList<Integer> arrayList, int key) { // else insertion point
        int l = 0, r = arrayList.size() - 1;
        while (l <= r) {
            int mid = (l + r) >>> 1;
            if (arrayList.get(mid) < key)
                l = mid + 1;
            else
                r = mid - 1;
        }
        return l;
    }

    private static int upperBoundBinarySearch(ArrayList<Integer> arrayList, int key) { // else insertion point - 1
        int l = 0, r = arrayList.size() - 1;
        while (l <= r) {
            int mid = (l + r) >>> 1;
            if (arrayList.get(mid) <= key)
                l = mid + 1;
            else
                r = mid - 1;
        }
        return r;
    }

    private static int gcd(int a, int b) {
        if (a == 0) return b;
        else return gcd(b % a, a);
    }
}
