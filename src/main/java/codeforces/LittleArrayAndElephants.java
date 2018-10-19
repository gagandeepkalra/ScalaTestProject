package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

/*

E. Little Elephant and Inversions :=> https://codeforces.com/contest/220/problem/E

Segment Trees, Two Pointers
(iterative segment tree)

 */
public class LittleArrayAndElephants {

    private static void insert(int idx, int[] segmentArr) {
        int n = segmentArr.length / 2;
        for (segmentArr[idx += n]++; idx > 1; idx >>= 1) segmentArr[idx >> 1] = segmentArr[idx] + segmentArr[idx ^ 1];
    }

    private static void remove(int idx, int[] segmentArr) {
        int n = segmentArr.length / 2;
        for (segmentArr[idx += n]--; idx > 1; idx >>= 1) segmentArr[idx >> 1] = segmentArr[idx] + segmentArr[idx ^ 1];
    }

    private static int query(int l, int r, int[] segmentArr) { // even => left child, odd => right child, [l, r)
        int res = 0, n = segmentArr.length / 2;
        for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
            if ((l & 1) == 1) res += segmentArr[l++];
            if ((r & 1) == 1) res += segmentArr[--r];
        }
        return res;
    }

    private static int[] compressed(String line, int n) {
        int[] rangedInput = new int[n];

        Map<Integer, Set<Integer>> indexOf = new HashMap<>(n);
        Set<Integer> set = new TreeSet<>();

        StringTokenizer st = new StringTokenizer(line);
        for (int i = 0; i < n; i++) {
            int input = Integer.parseInt(st.nextToken());

            Set<Integer> indexes = indexOf.getOrDefault(input, new TreeSet<>());
            indexes.add(i);
            indexOf.putIfAbsent(input, indexes);

            set.add(input);
        }

        int ctr = 0;
        for (Integer elem : set) { // sorted order
            for (Integer l : indexOf.get(elem)) {
                rangedInput[l] = ctr;
            }
            ctr++;
        }

        return rangedInput;
    }


    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());
        long k = Long.parseLong(st.nextToken());

        int[] input = compressed(br.readLine(), n);

        long inversions = 0, result = 0;

        int[] segmentArrL = new int[2 * n], segmentArrR = new int[2 * n]; // 0 is not used, 1 to n-1 are internal nodes, n to 2n-1 are leaves

        for (int i = 0; i < n; i++) {
            inversions += query(input[i] + 1, n, segmentArrL); // add #elements > input[i] in 0..i to inversion count
            insert(input[i], segmentArrL);
        }

        int l = n - 2, r = n - 1;

        // remove input[r] from left and move to right
        remove(input[r], segmentArrL);
        insert(input[r], segmentArrR);

        while (r > 0) {
            while (inversions > k && l >= 0) {
                remove(input[l], segmentArrL);
                inversions -= query(input[l] + 1, n, segmentArrL) + query(0, input[l], segmentArrR); // find #elements > input[l] in left and < input[l] in right
                l--;
            }

            result += l + 1;

            r--;
            insert(input[r], segmentArrR);

            if (l == r) {
                remove(input[l], segmentArrL);
                l--;
            } else {
                inversions += query(input[r] + 1, n, segmentArrL) + query(0, input[r], segmentArrR); // find #elements > input[l] in left and < input[l] in right
            }
        }

        System.out.println(result);

    }
}
