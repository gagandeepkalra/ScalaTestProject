package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/*

https://codeforces.com/contest/356/problem/A

 */
public class KnightTournament {

    private static int[] segmentArr; // count of killed in this segment

    private static int[] killedBy;

    private static void update(int l, int r, int i, int x, int y, int killer) {
        if (l > y || r < x || l > r || x > y) ;
        else {
            if (x <= l && r <= y) {
                if (segmentArr[i] == r - l + 1) return; // all dead
                if (l == r) {
                    segmentArr[i] = 1;
                    killedBy[l] = killer;
                    return;
                }
            }
            int m = (l + r) / 2;
            update(l, m, 2 * i, x, y, killer);
            update(m + 1, r, 2 * i + 1, x, y, killer);

            segmentArr[i] = segmentArr[2 * i] + segmentArr[2 * i + 1];
        }
    }


    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());
        int m = Integer.parseInt(st.nextToken());

        segmentArr = new int[sizeOfSegmentArr(n) + 1];
        killedBy = new int[n + 1];

        while (m-- > 0) {
            st = new StringTokenizer(br.readLine());
            int x = Integer.parseInt(st.nextToken()), y = Integer.parseInt(st.nextToken()), killer = Integer.parseInt(st.nextToken());
            update(1, n, 1, x, killer - 1, killer);
            update(1, n, 1, killer + 1, y, killer);
        }

        for (int i = 1; i <= n; i++) System.out.print(killedBy[i] + " ");
    }

    private static int sizeOfSegmentArr(int n) {
        return (int) (2 * Math.pow(2, Math.ceil(Math.log(n) / Math.log(2))) - 1);
    }
}
