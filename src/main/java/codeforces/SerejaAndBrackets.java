package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/*

https://codeforces.com/contest/380/problem/C

 */
public class SerejaAndBrackets {
    private static Pair[] segmentArr;

    private static void preCompute(int l, int r, int i, String input) {
        if (l == r) {
            segmentArr[i] = new Pair(input.charAt(l));
        } else {
            int m = (l + r) / 2;
            preCompute(l, m, 2 * i, input);
            preCompute(m + 1, r, 2 * i + 1, input);
            segmentArr[i] = segmentArr[2 * i].merge(segmentArr[2 * i + 1]);
        }
    }

    private static Pair query(int l, int r, int i, int x, int y) {
        if (y < l || x > r) return empty;
        else if (x <= l && r <= y) return segmentArr[i];
        else {
            int mid = (l + r) >> 1;
            return query(l, mid, 2 * i, x, y).merge(query(mid + 1, r, 2 * i + 1, x, y));
        }
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        final String input = st.nextToken();
        int n = input.length();

        segmentArr = new Pair[4 * n];
        preCompute(0, n - 1, 1, input);

        st = new StringTokenizer(br.readLine());
        int m = Integer.parseInt(st.nextToken());
        while (m-- > 0) {
            st = new StringTokenizer(br.readLine());
            int l = Integer.parseInt(st.nextToken()), r = Integer.parseInt(st.nextToken()); //  1 based

            Pair res = query(0, n - 1, 1, l - 1, r - 1);
            System.out.println((r - l + 1) - res.o - res.c); // length - unmatched brackets
        }
    }

    private static Pair empty = new Pair();

    private static class Pair {
        int o;
        int c;

        Pair() {
        }

        Pair(int o, int c) {
            this.o = o;
            this.c = c;
        }

        Pair(char bracket) {
            switch (bracket) {
                case '(':
                    o = 1;
                    break;
                case ')':
                    c = 1;
                    break;
            }
        }

        Pair merge(Pair that) {
            int t = Math.min(this.o, that.c);
            return new Pair(this.o + that.o - t, this.c + that.c - t);
        }

        @Override
        public String toString() {
            return "Pair{" +
                    "o=" + o +
                    ", c=" + c +
                    '}';
        }
    }
}
