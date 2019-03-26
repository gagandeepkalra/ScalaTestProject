package expedia;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;
import java.util.concurrent.ForkJoinPool;

public class NotebookRevision {

    private static final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));

    public static void main(String[] args) throws Exception {

        StringTokenizer stringTokenizer = new StringTokenizer(bufferedReader.readLine());

        int totalElements = Integer.parseInt(stringTokenizer.nextToken());
        long allElements[] = new long[totalElements];

        stringTokenizer = new StringTokenizer(bufferedReader.readLine());
        for (int i = 0; i < totalElements; i++) {
            allElements[i] = Long.parseLong(stringTokenizer.nextToken());
        }

        stringTokenizer = new StringTokenizer(bufferedReader.readLine());
        int k = Integer.parseInt(stringTokenizer.nextToken());

        if (k != 1) {
            final Map<Long, Integer> getIndexOf = new HashMap<>();
            for (int i = 0; i < totalElements; i++)
                if (!getIndexOf.containsKey(allElements[i])) getIndexOf.put(allElements[i], i); // first occurrence

            int[] sequenceLengthForEachElement = new int[totalElements];
            int result = Integer.MIN_VALUE;

            for (int i = 0; i < totalElements; i++) {
                result = Math.max(result, solveForIndexRecursively(i, sequenceLengthForEachElement, allElements, getIndexOf, k));
            }

            System.out.println(Arrays.stream(sequenceLengthForEachElement).mapToObj(String::valueOf).reduce((s, s2) -> s + " " + s2));

            //throw new NumberFormatException(totalElements + " " + k + "\n" + Arrays.stream(allElements).mapToObj(String::valueOf).reduce((s, s2) -> s + " " + s2).get());
            System.out.println(result);
        } else {
            final Map<Long, Integer> frequency = new HashMap<>();
            for (int i = 0; i < totalElements; i++) {
                frequency.put(allElements[i], frequency.getOrDefault(allElements[i], 0) + 1);
            }
            System.out.println(frequency.values().stream().max(Comparator.comparingInt(o -> o)).get());
        }
    }

    private static int solveForIndexRecursively(int idx, final int[] sequenceLength, final long[] arr, final Map<Long, Integer> indexOf, final int k) {
        if (sequenceLength[idx] == 0) {
            if (arr[idx] * k > arr[idx] && indexOf.containsKey(arr[idx] * k) && indexOf.get(arr[idx] * k) > idx) {
                solveForIndexRecursively(indexOf.get(arr[idx] * k), sequenceLength, arr, indexOf, k);
                sequenceLength[idx] = sequenceLength[indexOf.get(arr[idx] * k)];
            }
            sequenceLength[idx]++;
        }
        return sequenceLength[idx];
    }
}
