package codeforces.contests._1399;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.PriorityQueue;

/*
https://codeforces.com/contest/1399/problem/E2

[Graph Theory]

We calculate contribution of each edge by calculating number of leaves if we go down it. Afterwards, we use a max heap to
reduce the maximals of the weights only that the heap is ordered by diffs = w*c - w/2*c, courtesy integer division

Given two separate costs and the aim to find the cheapest mixture, we identify individual sequences, of c==1 and c==2
alone at a time.

Afterwards, we use two pointers to arrive at the best mix
 */
public class WeightsDivisionHardJava {

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        int T = Integer.parseInt(br.readLine());
        ArrayList<Integer> printBuffer = new ArrayList<>();
        for (int t = 1; t <= T; t++) {
            String[] l1 = br.readLine().split(" ");
            final int n = Integer.parseInt(l1[0]);
            final long s = Long.parseLong(l1[1]);

            ArrayList<int[]>[] graph = new ArrayList[n + 1];
            for (int i = 0; i <= n; i++) graph[i] = new ArrayList<>();

            {
                int nn = n - 1;
                while (nn-- > 0) {
                    String[] l2 = br.readLine().split(" ");
                    int u = Integer.parseInt(l2[0]);
                    int v = Integer.parseInt(l2[1]);
                    int w = Integer.parseInt(l2[2]);
                    int c = Integer.parseInt(l2[3]);

                    graph[u].add(new int[]{v, w, c});
                    graph[v].add(new int[]{u, w, c});
                }
            }

            PriorityQueue<int[]> queue = new PriorityQueue<>((o1, o2) -> (diff(o1) > diff(o2)) ? -1 : 1);

            final long sum = dfs(graph, queue, new int[n + 1], 1, 0);

            if (s >= sum) printBuffer.add(0);
            else {
                ArrayList<Long> ones = new ArrayList<>(), twos = new ArrayList<>();
                {
                    long sumOnes = sum;
                    long sumTwos = sum;
                    while (true) {
                        int[] pair = queue.poll();
                        int w = pair[0];

                        if (w == 0) break;

                        int l = pair[1];
                        int c = pair[2];
                        queue.add(new int[]{w / 2, l, c});

                        if (c == 1) {
                            sumOnes -= diff(pair);
                            ones.add(sumOnes);
                        } else {
                            sumTwos -= diff(pair);
                            twos.add(sumTwos);
                        }
                    }
                }

                int steps = Integer.MAX_VALUE;

                int j = 0;
                while (j < twos.size() && twos.get(j) > s) j++;
                if (j >= twos.size()) j = twos.size() - 1;

                if (j >= 0 && twos.get(j) <= s) steps = (j + 1) * 2;

                int i = 0;
                while (i < ones.size() && ones.get(i) > s) {
                    long restRemove = ones.get(i) - s;
                    while (j >= 0 && sum - twos.get(j) > restRemove) j--;
                    if (j + 1 < twos.size() && (j == -1 || sum - twos.get(j) < restRemove)) j++;

                    if (j >= 0 && sum - twos.get(j) >= restRemove) steps = Math.min(steps, i + 1 + (j + 1) * 2);
                    i++;
                }
                if (i >= ones.size()) i = ones.size() - 1;
                if (i >= 0 && ones.get(i) <= s) steps = Math.min(steps, i + 1);

                printBuffer.add(steps);
            }
        }

        System.out.println(printBuffer.stream().reduce(new StringBuffer(), (sb, i) -> sb.append(i).append("\n"), StringBuffer::append));
    }

    private static long diff(int[] pair) {
        int w = pair[0], l = pair[1];
        return (long) w * l - (long) w / 2 * l;
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
                    int c = pair[2];
                    total += dfs(graph, queue, leaves, v, u) + ((long) leaves[v]) * w;
                    leaves[u] += leaves[v];
                    queue.add(new int[]{w, leaves[v], c});
                }
            }
            return total;
        }
    }
}

