import java.util.*;

public class ShiftAndPush {
    public static void main(String[] args) {
        final Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int[] arr = new int[n];

        for (int i = 0; i < n; i++) {
            arr[i] = sc.nextInt();
        }

        // calculating frequency of each element in arr

        int[] freq = new int[100001]; // maximum value of each element is 100000

        for (int i = 0; i < n; i++) {
            freq[arr[i]]++;
        }

        int maxFrequency = Arrays.stream(freq).max().getAsInt();

        if (maxFrequency == 1) {
            System.out.println(n + n - 1);
        } else {

            Set<Integer> allElementsWithTheMostFrequency = new HashSet<>();

            for (int i = 0; i < 100001; i++) {
                if (freq[i] == maxFrequency) allElementsWithTheMostFrequency.add(i);
            }
            // find max distance between any two occurrences of the most frequenct element in the array

            int maxDistance = allElementsWithTheMostFrequency.stream().map(element -> maxDistanceBetweenOccurrences(arr, element)).min(Integer::compareTo).orElse(0);

            System.out.println(maxDistance + n);
        }
    }

    private static int maxDistanceBetweenOccurrences(int[] arr, int element) {
        ArrayList<Integer> allIndexes = new ArrayList<>();

        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                allIndexes.add(i);
            }
        }

        int maxDistance = Integer.MIN_VALUE;

        for (int i = 1; i < allIndexes.size(); i++) {
            maxDistance = Math.max(maxDistance, allIndexes.get(i) - allIndexes.get(i - 1) - 1);
        }

        maxDistance = Math.max(maxDistance, arr.length - 1 - allIndexes.get(allIndexes.size() - 1) + allIndexes.get(0));

        return maxDistance;
    }
}
