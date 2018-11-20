//package GoogleCodeJam2018RoundG;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

public class ProductTriplets {
    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int T = Integer.parseInt(st.nextToken());

        for (int t = 1; t <= T; t++) {
            st = new StringTokenizer(br.readLine());
            int n = Integer.parseInt(st.nextToken());

            int[] a = new int[n];

//            ArrayList<ArrayList<Integer>> elementToIndexesList = new ArrayList<>(1000000);
//            for (int i = 0; i < 1000000; i++) elementToIndexesList.add(new ArrayList<Integer>());

            st = new StringTokenizer(br.readLine());
            for (int i = 0; i < n; i++) a[i] = Integer.parseInt(st.nextToken());


            /*
            long k1 = ((long) a[i]) * ((long) a[j]);
                        long k2 = ((long) a[j]) * ((long) a[k]);
                        long k3 = ((long) a[k]) * ((long) a[i]);
                        if (k1 == a[k] || k2 == a[i] || k3 == a[j]) {
                            result++;
                        }
             */

            Arrays.sort(a);

//            for (int i = 0; i < n; i++) {
//                elementToIndexesList.get(a[i]).add(i);
//            }


            long result = 0;
            for (int i = 0; i < n; i++) {
                for (int j = i + 1; j < n; j++) {
                    //long key = ((long) a[i]) * ((long) a[j]);

//                    if (key < 1000000) {
//                        result += findCountOfElementsGreaterThan(elementToIndexesList.get(((int) key)), j);
//                    } else break;

                    for (int k = j + 1; k < n; k++) {
//                        if (key == a[k]) {
//                            result++;
//                        }

                        long k1 = ((long) a[i]) * ((long) a[j]);
                        long k2 = ((long) a[j]) * ((long) a[k]);
                        long k3 = ((long) a[k]) * ((long) a[i]);
                        if (k1 == a[k] || k2 == a[i] || k3 == a[j]) {
                            result++;
                        }
                    }
                }
            }
            System.out.println("Case #" + t + ": " + result);
        }

    }

    private static int findCountOfElementsGreaterThan(ArrayList<Integer> ls, int key) {
        int l = 0, r = ls.size() - 1;
        while (l <= r) {
            int mid = (l + r) >>> 1;
            if (ls.get(mid) <= key)
                l = mid + 1;
            else
                r = mid - 1;
        }

        return ls.size() - l;

    }
}
