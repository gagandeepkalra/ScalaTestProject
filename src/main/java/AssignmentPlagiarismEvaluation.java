import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Collectors;

public class AssignmentPlagiarismEvaluation {
    public static void main(String[] args) {
        final Scanner sc = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
        final int n = sc.nextInt(), threshold = sc.nextInt();

        Map<Integer, Map<String, Integer>> rollNoToWordCount = new HashMap<>();

        for (int i = 0; i < n; i++) {
            int rollNo = sc.nextInt(), m = sc.nextInt();
            String[] lines = new String[m];

            sc.nextLine();
            for (int j = 0; j < m; j++) lines[j] = sc.nextLine();

            rollNoToWordCount.put(rollNo, wordCount(lines));
        }

        checkPlagiarism(threshold, rollNoToWordCount).stream().sorted(Comparator.comparingInt(Set::size)).forEach(set -> {
            set.stream().sorted().forEach(i -> System.out.print(i + " "));
            System.out.println();
        });


    }

    private static boolean isPalgiarised(Map<String, Integer> a, Map<String, Integer> b, int threshold) {
        Set<String> common = new HashSet<>(a.keySet());

        common.retainAll(b.keySet());

        long matchingCount = common.stream().map(word -> {
            int aCount = a.get(word), bCount = b.get(word);
            return (Math.min(aCount, bCount) * 1.0) / Math.max(aCount, bCount) * 100;
        }).filter(aDouble -> aDouble >= threshold).count();

        common.addAll(a.keySet());
        common.addAll(b.keySet());

        double percent = ((matchingCount * 1.0) / common.size()) * 100;
        if (percent >= threshold) {
            return true;
        } else return false;
    }


    private static Set<Set<Integer>> checkPlagiarism(final int threshold, final Map<Integer, Map<String, Integer>> rollNoToWordCount) {

        Set<Integer> result = new HashSet<>();

        Integer[] rollNos = ((Integer[]) rollNoToWordCount.keySet().stream().sorted().toArray());


        Map<Integer, Set<Integer>> graph = new HashMap<>();

        for (int i = 0; i < rollNos.length; i++) {

            Set<Integer> neighbors = new HashSet<>();

            for (int j = 0; j < rollNos.length; j++) {
                if (i != j) {
                    if (isPalgiarised(rollNoToWordCount.get(rollNos[i]), rollNoToWordCount.get(rollNos[j]), threshold)) {
                        neighbors.add(j);
                    }
                }
            }

            graph.put(i, neighbors);
        }

        Set<Set<Integer>> connectedComponents = new HashSet<>();
        Set<Integer> visited = new HashSet<>();

        for (int i = 0; i < rollNos.length; i++) {
            if (!visited.contains(i)) {
                Set<Integer> res = new HashSet<>();
                findAllConnected(graph, i, res);
                visited.addAll(res);

                connectedComponents.add(res);
            }
        }

        return connectedComponents.stream().map(sets -> sets.stream().map(i -> rollNos[i]).collect(Collectors.toSet())).collect(Collectors.toSet());
    }

    private static void findAllConnected(Map<Integer, Set<Integer>> graph, int root, Set<Integer> result) {
        if (!result.contains(root)) {
            result.add(root);
            Set<Integer> neighbors = graph.get(root);

            if (neighbors != null) {
                neighbors.forEach(integer -> findAllConnected(graph, integer, result));
            }
        }
    }

    private static Map<String, Integer> wordCount(String[] lines) {
        Map<String, Integer> result = new HashMap<>();

        Arrays.stream(lines)
                .forEach(line -> Arrays.stream(line.split(" ")).filter(s -> !s.equals(""))
                        .forEach(word -> result.put(word.toLowerCase(), result.getOrDefault(word, 0) + 1)));

        return result;
    }
}
