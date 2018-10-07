package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;

public class CopyingData {
    private static int[][] segmentArr; // 1 based

    private static void apply(int i, int value0, int value1) {
        segmentArr[i][0] = value0;
        segmentArr[i][1] = value1;
    }

    private static boolean updatePresent(int i) {
        return segmentArr[i][0] != -1 || segmentArr[i][1] != -1;
    }

    private static void update(int l, int r, int i, int x, int y, int value0, int value1) { // inclusive
        if (r < x || y < l) return;

        if (x <= l && r <= y) {
            apply(i, value0, value1);
            return;
        }

        // push down current update
        if (updatePresent(i)) {
            apply(2 * i, segmentArr[i][0], segmentArr[i][1]);
            apply(2 * i + 1, segmentArr[i][0], segmentArr[i][1]);
            apply(i, -1, -1);
        }

        int m = (l + r) / 2;
        update(l, m, 2 * i, x, y, value0, value1);
        update(m + 1, r, 2 * i + 1, x, y, value0, value1);
    }

    private static int query(int l, int r, int i, int idx) { // returns index in array A or -1 if result from array B
        if (updatePresent(i)) return segmentArr[i][0] + idx - segmentArr[i][1];
        if (l == r) return -1; // end of line

        int m = (l + r) / 2;
        if (idx <= m) return query(l, m, 2 * i, idx);
        else return query(m + 1, r, 2 * i + 1, idx);
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken()), q = Integer.parseInt(st.nextToken());

        int arr[] = new int[n + 1], brr[] = new int[n + 1];
        st = new StringTokenizer(br.readLine());
        for (int i = 1; i <= n; i++) arr[i] = Integer.parseInt(st.nextToken());
        st = new StringTokenizer(br.readLine());
        for (int i = 1; i <= n; i++) brr[i] = Integer.parseInt(st.nextToken());

        segmentArr = new int[4 * n][2];
        for (int[] row : segmentArr) Arrays.fill(row, -1);

        while (q-- > 0) {
            st = new StringTokenizer(br.readLine());
            switch (Integer.parseInt(st.nextToken())) {
                case 1:
                    int a = Integer.parseInt(st.nextToken()), b = Integer.parseInt(st.nextToken()), k = Integer.parseInt(st.nextToken());
                    update(1, n, 1, b, b + k - 1, a, b);
                    break;
                case 2:
                    int idx = Integer.parseInt(st.nextToken());
                    int res = query(1, n, 1, idx);
                    if (res == -1) System.out.println(brr[idx]);
                    else System.out.println(arr[res]);
                    break;
            }
        }

    }
}
