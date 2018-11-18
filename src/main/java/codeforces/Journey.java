package codeforces;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

/*

http://codeforces.com/problemset/problem/839/C
DFS

 */
public class Journey {
    public static void main(String[] args) throws Exception {
        final BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(bf.readLine());

        int n = Integer.parseInt(st.nextToken());

        final Map<Integer, Set<Integer>> graph = new HashMap<>();
        graph.put(1, new HashSet<Integer>() {{
            add(0);
        }});

        while (n-- > 1) {
            st = new StringTokenizer(bf.readLine());
            int x = Integer.parseInt(st.nextToken()), y = Integer.parseInt(st.nextToken());

            Set<Integer> valuesX = graph.getOrDefault(x, new HashSet<>());
            valuesX.add(y);
            Set<Integer> valuesY = graph.getOrDefault(y, new HashSet<>());
            valuesY.add(x);

            graph.putIfAbsent(x, valuesX);
            graph.putIfAbsent(y, valuesY);
        }

        System.out.println(calculateExpectedDistance(graph, 1, 0, 0, 1.0));
    }

    private static double calculateExpectedDistance(Map<Integer, Set<Integer>> graph, int me, int parent, int distance, double probability) {
        Set<Integer> children = graph.get(me);
        if (children.size() == 1) {
            return probability * distance;
        } else {
            Double acc = 0.0;
            int childCount = children.size() - 1;
            for (Integer child : children) {
                if (child != parent) {
                    acc += calculateExpectedDistance(graph, child, me, distance + 1, probability * (1.0 / childCount));
                }
            }
            return acc;
        }
    }
}
