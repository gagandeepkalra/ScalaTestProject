package codeforces.contests._1399;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.PriorityQueue;

/*
https://codeforces.com/contest/1399/problem/E1

[Graph Theory]

We calculate contribution of each edge by calculating number of leaves if we go down it. Afterwards, we use a max heap to
reduce the maximals of the weights only that the heap is ordered by diffs = w*c - w/2*c, courtesy integer division

Scala version didn't pass, TLE
*/
public class WeightsDivisionEasyJava {

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        int t = Integer.parseInt(br.readLine());
        ArrayList<Integer> printBuffer = new ArrayList<>();
        while (t-- > 0) {
            String[] l1 = br.readLine().split(" ");
            int n = Integer.parseInt(l1[0]);
            long s = Long.parseLong(l1[1]);

            ArrayList<int[]>[] graph = new ArrayList[n + 1];
            for (int i = 0; i <= n; i++) graph[i] = new ArrayList<>();

            {
                int nn = n - 1;
                while (nn-- > 0) {
                    String[] l2 = br.readLine().split(" ");
                    int u = Integer.parseInt(l2[0]);
                    int v = Integer.parseInt(l2[1]);
                    int w = Integer.parseInt(l2[2]);

                    graph[u].add(new int[]{v, w});
                    graph[v].add(new int[]{u, w});
                }
            }

            PriorityQueue<int[]> queue = new PriorityQueue<>((o1, o2) -> (diff(o1) > diff(o2)) ? -1 : 1);
            int[] leaves = new int[n + 1];

            long sum = dfs(graph, queue, leaves, 1, 0);

            int steps = 0;

            while (sum > s) {
                int[] pair = queue.poll();
                int w = pair[0];
                int c = pair[1];
                queue.add(new int[]{w / 2, c});
                sum -= diff(pair);
                steps += 1;
            }

            printBuffer.add(steps);
        }

        System.out.println(printBuffer.stream().reduce(new StringBuffer(), (sb, i) -> sb.append(i).append("\n"), StringBuffer::append));
    }

    private static long diff(int[] pair) {
        int w = pair[0], c = pair[1];
        return (long) w * c - (long) w / 2 * c;
    }

    private static long dfs(ArrayList<int[]>[] graph, PriorityQueue<int[]> queue, int[] leaves, int u, int p) {
        if (graph[u].size() == 1 && graph[u].get(0)[0] == p) {
            leaves[u] = 1;
            return 0L;
        } else {
            long total = 0;
            for (int[] pair : graph[u]) {
                int v = pair[0];
                if (v != p) {
                    int w = pair[1];
                    total += dfs(graph, queue, leaves, v, u) + ((long) leaves[v]) * w;
                    leaves[u] += leaves[v];
                    queue.add(new int[]{w, leaves[v]});
                }
            }
            return total;
        }
    }
}
