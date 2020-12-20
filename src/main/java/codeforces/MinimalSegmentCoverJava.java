package codeforces;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.IntStream;

public class MinimalSegmentCoverJava {

    private static class Pair {
        final int r, value;

        public Pair(int r, int value) {
            this.r = r;
            this.value = value;
        }

        public int getR() {
            return r;
        }

        public int getValue() {
            return value;
        }
    }

    private final static Map<Integer, Pair> intervalToRAndIndexMap = new HashMap<>(500000), indexToRAndResultMap = new HashMap<>(500000);

    public static void main(String[] args) throws IOException {

        final BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

        StringTokenizer st = new StringTokenizer(bf.readLine());
        final int n = Integer.parseInt(st.nextToken()), m = Integer.parseInt(st.nextToken());

        int maxR = Integer.MIN_VALUE;

        for (int i = 0; i < n; i++) {
            st = new StringTokenizer(bf.readLine());
            int x = Integer.parseInt(st.nextToken()), y = Integer.parseInt(st.nextToken());

            Pair newPair = new Pair(y, i);

            Pair maxPair = Optional.ofNullable(intervalToRAndIndexMap.get(x)).map(currPair -> {
                if (currPair.r >= y)
                    return currPair;
                else {
                    indexToRAndResultMap.remove(currPair.value);
                    return newPair;
                }
            }).orElse(newPair);

            intervalToRAndIndexMap.put(x, maxPair);
            indexToRAndResultMap.put(maxPair.value, new Pair(maxPair.r, 1));

            maxR = Math.max(maxR, maxPair.r);
        }

        ArrayList<Optional<Integer>> rightmostIntervalsIndex = new ArrayList<>(maxR + 1);


        rightmostIntervalsIndex.add(Optional.ofNullable(intervalToRAndIndexMap.get(0)).map(Pair::getValue));
        for (int i = 1; i <= maxR; i++) {
            Optional<Pair> prevPair = rightmostIntervalsIndex.get(i - 1).map(index -> new Pair(indexToRAndResultMap.get(index).r, index));
            Optional<Pair> currPair = Optional.ofNullable(intervalToRAndIndexMap.get(i));

            final int index = i;

            rightmostIntervalsIndex.add(
                    Optional.ofNullable(currPair.flatMap(cP -> prevPair.map(pP -> {
                        if (cP.r >= pP.r)
                            return cP;
                        else
                            return pP;
                    })).orElse(prevPair.orElse(currPair.orElse(null))))
                            .filter(pair -> pair.r >= index)
                            .map(Pair::getValue)
            );


        }

        int[][] queries = new int[m][2];
        for (int i = 0; i < m; i++) {
            st = new StringTokenizer(bf.readLine());
            int x = Integer.parseInt(st.nextToken()), y = Integer.parseInt(st.nextToken());
            queries[i][0] = x;
            queries[i][1] = y;
        }

        int[] result = new int[m];

        final int maximumR = maxR;

        IntStream.range(0, m)
                .boxed()
                .sorted(Comparator.comparingInt(o -> queries[o][1]))
                .forEach(qi -> {
                    int ql = queries[qi][0], qr = queries[qi][1];
                    if (ql <= maximumR && qr <= maximumR) {
                        pathCompression(ql, qr, rightmostIntervalsIndex);

                        result[qi] = rightmostIntervalsIndex.get(ql)
                                .flatMap(index -> Optional.ofNullable(indexToRAndResultMap.get(index)))
                                .filter(pair -> pair.r >= qr)
                                .map(Pair::getValue)
                                .orElse(-1);
                    } else {
                        result[qi] = -1;
                    }

                });

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < m; i++) sb.append(result[i]).append("\n");

        System.out.println(sb.toString());
    }

    private static void pathCompression(int l, int r, ArrayList<Optional<Integer>> rightmostIntervalsIndex) {
        rightmostIntervalsIndex.get(l).ifPresent(index ->
                Optional.ofNullable(indexToRAndResultMap.get(index)).ifPresent(pair -> {
                    int rightmost = pair.r, steps = pair.value;
                    if (rightmost < r && rightmost != l) {
                        pathCompression(rightmost, r, rightmostIntervalsIndex);

                        rightmostIntervalsIndex.get(rightmost).flatMap(indexR -> Optional.ofNullable(indexToRAndResultMap.get(indexR)))
                                .ifPresent(pairR ->
                                        indexToRAndResultMap.put(index, new Pair(pairR.r, steps + pairR.value))
                                );
                    }
                })
        );


    }
}
