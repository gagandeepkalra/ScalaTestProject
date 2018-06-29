import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;
/*
Graph
 */
public class BobAndForest {
    public static void main(String[] args) throws Exception {

        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken()), m = Integer.parseInt(st.nextToken());
        int[][] grid = new int[n][m];

        int[] count = new int[n + 1];

        for (int i = 0; i < n; i++) {
            final String row = new StringTokenizer(br.readLine()).nextToken();
            for (int j = 0; j < m; j++) {
                if (row.charAt(j) == '.') grid[i][j] = 0;
                else if (row.charAt(j) == '*') {
                    grid[i][j] = 1;
                } else {
                    System.err.println("Illegal Character");
                    return;
                }
            }
        }

        for (int i = n - 2; i >= 0; i--) {
            for (int j = m - 2; j >= 0; j--) {
                if (grid[i][j] != 0) {
                    grid[i][j] += Math.min(grid[i + 1][j], Math.min(grid[i + 1][j + 1], grid[i][j + 1]));
                    count[grid[i][j]]++;
                }
            }
        }

        for (int i = 0; i < n; i++) {
            if (grid[i][m - 1] == 1) count[1]++;
        }

        for (int j = 0; j < m; j++) {
            if (grid[n - 1][j] == 1) count[1]++;
        }

        if (grid[n - 1][m - 1] == 1) count[1]--;

//        for (int i = 0; i < n; i++) {
//            for (int j = 0; j < m; j++) {
//                System.out.print(grid[i][j] + " ");
//            }
//            System.out.println();
//        }
//
//        System.out.println();
//
//        for (int i = 0; i <= n; i++) {
//            System.out.print(count[i] + " ");
//        }

        for (int i = n, sum = 0; i > 0; i--) {
            if (count[i] != 0) {
                sum += count[i];
                count[i] += sum - count[i];
            }
        }

//        System.out.println();
//
//        for (int i = 0; i <= n; i++) {
//            System.out.print(count[i] + " ");
//        }

        for (int i = 1; i <= n; i++) {
            count[i] += count[i - 1];
        }

//        System.out.println();
//
//        for (int i = 0; i <= n; i++) {
//            System.out.print(count[i] + " ");
//        }

        int k = Integer.parseInt(new StringTokenizer(br.readLine()).nextToken());
        while (k-- > 0) {
            int i = Math.max(Integer.parseInt(new StringTokenizer(br.readLine()).nextToken()), n);
            System.out.println(count[i]);
        }
    }
}
