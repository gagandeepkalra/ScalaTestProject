package GoogleCodeJam2019RoundA;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Scanner;

public class Training {
    public static void main(String[] args) {
        final Scanner sc = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
        int cases = sc.nextInt();

        for (int t = 1; t <= cases; t++) {
            int n = sc.nextInt(), p = sc.nextInt();

            int[] arr = new int[n];
            for (int i = 0; i < n; i++) arr[i] = sc.nextInt();
            Arrays.sort(arr);

            int[] prefixSum = new int[n];
            prefixSum[0] = arr[0];
            for (int i = 1; i < n; i++) prefixSum[i] = prefixSum[i - 1] + arr[i];

            int result = arr[p - 1] * p - prefixSum[p - 1];

            for (int i = p; i < n; i++) {
                result = Math.min(result, arr[i] * p - (prefixSum[i] - prefixSum[i - p]));
            }

            System.out.println(String.format("Case #%d: %d", t, result));
        }
    }
}
