import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;

/*
You are given an array A of length N. You have to choose a subset S from given array A, such that average of S is less than K. You need to print the maximum possible length of S.
 */
public class HighestAverage {

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());

        st = new StringTokenizer(br.readLine());
        int[] arr = new int[n];
        for (int i = 0; i < n; i++) arr[i] = Integer.parseInt(st.nextToken());

        Arrays.sort(arr);

        double[] avg = new double[n];
        avg[0] = arr[0];
        for (int i = 1; i < n; i++) avg[i] = (avg[i - 1] * i + arr[i]) / (i + 1);

        st = new StringTokenizer(br.readLine());
        int q = Integer.parseInt(st.nextToken());

        while (q-- > 0) {
            long k = Integer.parseInt(br.readLine());

            int idx = Arrays.binarySearch(avg, k);

            if (idx < 0) {
                idx = (idx * -1) - 1;
            } else if (idx > 0 && avg[idx] == avg[idx - 1]) {
                double temp = avg[idx];
                while (idx > 0 && avg[idx] == temp) {
                    idx--;
                }
            }

            System.out.println(idx);
        }
    }
}
