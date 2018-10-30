package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.function.BiFunction;

/*

D. Points :=> https://codeforces.com/contest/19/problem/D
2d plane search

 */
public class Points {
    private static int[] compressed(int[] x) {
        int n = x.length;
        int[] rangedInput = new int[n];

        Map<Integer, Set<Integer>> indexOf = new HashMap<>(n);
        Set<Integer> set = new TreeSet<>();

        for (int i = 0; i < n; i++) {
            int input = x[i];

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

    private static void update(int[] segmentArr, int idx, int value, int l, int r, int i, BiFunction<Integer, Integer, Integer> f) { // 1 based segmentArr
        if (idx < l || r < idx) return;
        else if (l == r) segmentArr[i] = f.apply(segmentArr[i], value);
        else {
            int m = (l + r) >>> 1;
            update(segmentArr, idx, value, l, m, i << 1, f);
            update(segmentArr, idx, value, m + 1, r, (i << 1) | 1, f);

            segmentArr[i] = Math.max(segmentArr[i << 1], segmentArr[(i << 1) | 1]);
        }
    }

    private static int find(int[] segmentArr, int idx, int key, int l, int r, int i) { // first index >= idx (to the right) that has value >= key
        if (idx > r || key > segmentArr[i])
            return -1;
        else if (l == r)
            return l;
        else {
            int m = (l + r) >>> 1;
            int result = find(segmentArr, idx, key, l, m, i << 1);
            if (result == -1)
                return find(segmentArr, idx, key, m + 1, r, (i << 1) | 1);
            else
                return result;
        }
    }

    enum Words {
        add,
        remove,
        find
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());

        final Words[] words = new Words[n];
        final int[] x = new int[n], y = new int[n];

        for (int i = 0; i < n; i++) {
            st = new StringTokenizer(br.readLine());

            words[i] = Words.valueOf(st.nextToken());
            x[i] = Integer.parseInt(st.nextToken());
            y[i] = Integer.parseInt(st.nextToken());
        }

        int[] cX = compressed(x);
        final Map<Integer, Integer> compressedXtoX = new HashMap<>(n);

        int maxX = Integer.MIN_VALUE;
        for (int i = 0; i < n; ++i) {
            compressedXtoX.put(cX[i], x[i]);
            maxX = Math.max(maxX, cX[i]);
        }

        int[] segmentArr = new int[4 * maxX];

        final Map<Integer, TreeSet<Integer>> xToYs = new HashMap<>(maxX);

        for (int i = 0; i < n; i++) {
            switch (words[i]) {
                case add:
                    updateMap(xToYs, cX[i], y[i]);
                    update(segmentArr, cX[i], y[i], 0, maxX, 1, Math::max);
                    break;
                case remove:
                    TreeSet<Integer> ys = xToYs.get(cX[i]);
                    ys.remove(y[i]);
                    update(segmentArr, cX[i], ys.isEmpty() ? 0 : ys.last(), 0, maxX, 1, (a, b) -> b);
                    break;
                default:
                    int cx = find(segmentArr, cX[i] + 1, y[i] + 1, 0, maxX, 1);
                    System.out.println(cx == -1 ? -1 : compressedXtoX.get(cx) + " " + xToYs.get(cx).ceiling(y[i] + 1));
            }
        }
    }

    private static void updateMap(Map<Integer, TreeSet<Integer>> map, int key, int value) {
        TreeSet<Integer> result = map.getOrDefault(key, new TreeSet<>());
        result.add(value);
        map.putIfAbsent(key, result);
    }
}
