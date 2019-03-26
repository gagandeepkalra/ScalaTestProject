import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class PackageConflicts {

    public static void main(String[] args) {
        final Scanner sc = new Scanner(new BufferedReader(new InputStreamReader(System.in)));

        int n = sc.nextInt();

        Map<String, String[]> graph = new HashMap<>();
        sc.nextLine();

        while (n-- > 0) {
            String[] full = sc.nextLine().split("=>");
            String parent = full[0];


            String[] neighbors;

            if (full.length == 2) {
                neighbors = full[1].split(",");
            } else {
                neighbors = new String[0];
            }

            graph.put(parent, neighbors);
        }

        Set<String> conflictingSet = new HashSet<>(Arrays.asList(sc.nextLine().split(",")));
        Set<String> fullDependencySet = new HashSet<>();

        for (String s : sc.nextLine().split(",")) {
            findAllDependencies(s, graph, fullDependencySet);
        }

        fullDependencySet.retainAll(conflictingSet);

        if (fullDependencySet.size() <= 1) {
            System.out.println("No conflict");
        } else {
            System.out.println("Conflict found");
        }
    }

    private static void findAllDependencies(String root, Map<String, String[]> graph, Set<String> result) {
        result.add(root);
        Arrays.stream(graph.get(root)).forEach(s -> findAllDependencies(s, graph, result));
    }
}
