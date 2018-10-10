package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/*

https://codeforces.com/contest/501/problem/D

https://en.wikipedia.org/wiki/Factorial_number_system

 */
public class MishaAndPermutationsSummation {

    private static void build(int[] seg, int l, int r, int i) {
        if (l == r) seg[i] = 1;
        else {
            int m = (l + r) >>> 1;

            build(seg, l, m, 2 * i + 1);
            build(seg, m + 1, r, 2 * i + 2);

            seg[i] = seg[2 * i + 1] + seg[2 * i + 2];
        }
    }

    private static int findRankOf(int[] seg, int l, int r, int i, int element) {
        if (l == r) return 0;
        else {
            int m = (l + r) >>> 1;

            if (element <= m) return findRankOf(seg, l, m, 2 * i + 1, element);
            else return seg[2 * i + 1] + findRankOf(seg, m + 1, r, 2 * i + 2, element);
        }
    }

    private static int findElementWithRank(int[] seg, int l, int r, int i, int rank) { // 0 based
        if (l == r) return l;
        else {
            int m = (l + r) >>> 1;
            if (rank < seg[2 * i + 1]) return findElementWithRank(seg, l, m, 2 * i + 1, rank);
            else return findElementWithRank(seg, m + 1, r, 2 * i + 2, rank - seg[2 * i + 1]);
        }
    }

    private static void removeElement(int[] seg, int l, int r, int i, int element) {
        if (l == r) seg[i] = 0;
        else {
            int m = (l + r) >>> 1;

            if (element <= m) removeElement(seg, l, m, 2 * i + 1, element);
            else removeElement(seg, m + 1, r, 2 * i + 2, element);

            seg[i] = seg[2 * i + 1] + seg[2 * i + 2];
        }
    }

    private static int[] toFactorialNumberSystem(int[] in) {
        int n = in.length;
        int[] factorodic = new int[n], seg = new int[4 * n];

        build(seg, 0, n - 1, 0);

        for (int i = 0; i < n; i++) {
            factorodic[i] = findRankOf(seg, 0, n - 1, 0, in[i]);
            removeElement(seg, 0, n - 1, 0, in[i]);
        }
        return factorodic;
    }

    private static int[] toPermutationFromFactorialNumberSystem(int[] in) {
        int n = in.length;
        int[] result = new int[n], seg = new int[4 * n];

        build(seg, 0, n - 1, 0);

        for (int i = 0; i < n; i++) {
            result[i] = findElementWithRank(seg, 0, n - 1, 0, in[i]);
            removeElement(seg, 0, n - 1, 0, result[i]);
        }
        return result;
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());

        int[] p1 = new int[n], p2 = new int[n];

        st = new StringTokenizer(br.readLine());
        for (int i = 0; i < n; i++) p1[i] = Integer.parseInt(st.nextToken());
        st = new StringTokenizer(br.readLine());
        for (int i = 0; i < n; i++) p2[i] = Integer.parseInt(st.nextToken());

        int[] factorodic1 = toFactorialNumberSystem(p1), factorodic2 = toFactorialNumberSystem(p2), addition = new int[n];

        for (int i = n - 1, j = 0, carry = 0; i >= 0; i--, j++) {
            int sum = factorodic1[i] + factorodic2[i] + carry;
            addition[i] = sum % (j + 1);
            carry = sum / (j + 1);
        }

        int[] result = toPermutationFromFactorialNumberSystem(addition);
        for (int i = 0; i < n; i++) {
            System.out.print(result[i] + " ");
        }


    }
}
