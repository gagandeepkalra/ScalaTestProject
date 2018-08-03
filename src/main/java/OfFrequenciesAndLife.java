import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.StringTokenizer;
import java.util.TreeSet;

public class OfFrequenciesAndLife {


    /*

    Answer n Queries of type:
    1. 1 elem -> increase frequency of elem by one
    2. 2 elem -> decrease frequency of elem by one
    3. 3 -> from the set of elements having the least frequency, print out the maximum element
    4. 4 -> from the set of elements having the hoighest frequency, print out the least element

     */
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());

        Map<Integer, Integer> elementToFrequency = new HashMap<>();
        Map<Integer, TreeSet<Integer>> frequencyToElements = new HashMap<>();

        SortedSet<Integer> allFrequenciesPresent = new TreeSet<>();

        while (n-- > 0) {
            st = new StringTokenizer(br.readLine());
            int element, currentFrequency, newFrequency;
            switch (Integer.parseInt(st.nextToken())) {
                case 1: {
                    element = Integer.parseInt(st.nextToken());
                    currentFrequency = elementToFrequency.getOrDefault(element, 0);
                    newFrequency = currentFrequency + 1;

                    frequencyToElements.getOrDefault(currentFrequency, new TreeSet<>()).remove(element);

                    elementToFrequency.put(element, newFrequency);
                    allFrequenciesPresent.add(newFrequency);

                    if (frequencyToElements.getOrDefault(currentFrequency, new TreeSet<>()).size() == 0)
                        allFrequenciesPresent.remove(currentFrequency);

                    TreeSet<Integer> x = frequencyToElements.getOrDefault(newFrequency, new TreeSet<>());
                    x.add(element);
                    frequencyToElements.put(newFrequency, x);

                    break;
                }
                case 2: {
                    element = Integer.parseInt(st.nextToken());
                    currentFrequency = elementToFrequency.getOrDefault(element, 0);
                    newFrequency = currentFrequency - 1;

                    frequencyToElements.getOrDefault(currentFrequency, new TreeSet<>()).remove(element);

                    if (frequencyToElements.getOrDefault(currentFrequency, new TreeSet<>()).size() == 0)
                        allFrequenciesPresent.remove(currentFrequency);

                    if (newFrequency <= 0) {
                        elementToFrequency.remove(element);
                    } else {
                        elementToFrequency.put(element, newFrequency);
                        allFrequenciesPresent.add(newFrequency);
                        TreeSet<Integer> x = frequencyToElements.getOrDefault(newFrequency, new TreeSet<>());
                        x.add(element);
                        frequencyToElements.put(newFrequency, x);
                    }

                    break;
                }
                case 3: {
                    if (allFrequenciesPresent.size() > 0) {
                        TreeSet<Integer> result = frequencyToElements.getOrDefault(allFrequenciesPresent.first(), new TreeSet<>());
                        if (result.size() > 0) {
                            System.out.println(result.last());
                        } else System.out.println(-1);
                    } else System.out.println(-1);
                }
                break;
                case 4: {
                    if (allFrequenciesPresent.size() > 0) {
                        TreeSet<Integer> result = frequencyToElements.getOrDefault(allFrequenciesPresent.last(), new TreeSet<>());
                        if (result.size() > 0) {
                            System.out.println(result.first());
                        } else System.out.println(-1);
                    } else System.out.println(-1);
                }
            }
        }
    }
}
