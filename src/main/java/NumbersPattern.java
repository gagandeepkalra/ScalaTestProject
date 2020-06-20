import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.Scanner;

/*
Given a pattern of N's and M's, M => decrease, N => increase by 1 each. Find a sequence of non repeating int s [1 .. 9]
that satisfy the constraint

[Backtracking]
 */
public class NumbersPattern {

    static int findFirstAvailableDownward(boolean[] visited, int starting) {
        for (int i = starting; i >= 1; i--) {
            if (!visited[i]) return i;
        }
        return -1;
    }

    static int findFirstAvailableUpward(boolean[] visited, int starting) {
        for (int i = starting; i <= 9; i++) {
            if (!visited[i]) return i;
        }
        return -1;
    }

    static int lastDigit(int number) {
        return Math.abs(number) % 10;
    }

    static int recur(boolean[] visited, String pattern, int pi, int result) {
        if (pi >= pattern.length()) return result;
        else {
            int prev = lastDigit(result);
            switch (pattern.charAt(pi)) {
                case 'M': {
                    if (prev == 1) return -1;
                    int next = findFirstAvailableDownward(visited, prev - 1);

                    while (next != -1) {
                        visited[next] = true;
                        int ans = recur(visited, pattern, pi + 1, result * 10 + next);
                        if (ans != -1) return ans;
                        visited[next] = false;
                        next = findFirstAvailableDownward(visited, next - 1);
                    }
                    return -1;
                }
                case 'N': {
                    if (prev == 9) return -1;
                    int next = findFirstAvailableUpward(visited, prev + 1);

                    while (next != -1) {
                        visited[next] = true;
                        int ans = recur(visited, pattern, pi + 1, result * 10 + next);
                        if (ans != -1) return ans;
                        visited[next] = false;
                        next = findFirstAvailableUpward(visited, next + 1);
                    }
                    return -1;
                }
                default:
                    return -1;
            }
        }
    }

    static int findPossibleSmallestNumberMatchingPattern(String pattern) {
        if (pattern.isEmpty()) return -1;

        boolean[] visited = new boolean[10];
        Arrays.fill(visited, false);

        for (int i = 1; i <= 9; i++) {
            Arrays.fill(visited, false);
            visited[i] = true;
            int result = recur(visited, pattern, 0, i);
            if (result != -1) return result;
        }
        return -1;
    }


    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        final String fileName = System.getenv("OUTPUT_PATH");
        BufferedWriter bw = null;
        if (fileName != null) {
            bw = new BufferedWriter(new FileWriter(fileName));
        } else {
            bw = new BufferedWriter(new OutputStreamWriter(System.out));
        }

        int res;
        String pattern;
        try {
            pattern = in.nextLine();
        } catch (Exception e) {
            pattern = null;
        }

        res = findPossibleSmallestNumberMatchingPattern(pattern);
        bw.write(String.valueOf(res));
        bw.newLine();

        bw.close();
    }
}