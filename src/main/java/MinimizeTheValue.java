import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.StringTokenizer;

/**
 * Dijkstra
 */
public class MinimizeTheValue {

    static class Node {
        int v;
        long w;
        long cost = Long.MAX_VALUE;

        Node(int v, long w) {
            this.v = v;
            this.w = w;
        }

        Node(int v, long w, long cost) {
            this.v = v;
            this.w = w;
            this.cost = cost;
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken()), m = Integer.parseInt(st.nextToken()), f = Integer.parseInt(st.nextToken());

        st = new StringTokenizer(br.readLine());
        int[] parkingCapacity = new int[n + 1];
        for (int i = 1; i <= n; i++) parkingCapacity[i] = Integer.parseInt(st.nextToken());

        LinkedList<Node>[] adjList = new LinkedList[n + 1];
        for (int i = 1; i <= n; i++) {
            adjList[i] = new LinkedList<>();
        }

        while (m-- > 0) {
            st = new StringTokenizer(br.readLine());
            int u = Integer.parseInt(st.nextToken()), v = Integer.parseInt(st.nextToken());
            long w = Long.parseLong(st.nextToken());
            adjList[u].add(new Node(v, w));
            adjList[v].add(new Node(u, w));
        }

        PriorityQueue<Node> queue = new PriorityQueue<>(Comparator.comparingLong(o -> o.cost));

        st = new StringTokenizer(br.readLine());
        int k = Integer.parseInt(st.nextToken());
        boolean[] visited = new boolean[n + 1];


        queue.add(new Node(1, 0, 0));

        while (!queue.isEmpty()) {
            Node node = queue.poll();

            if (visited[node.v]) continue;

            int capacity = parkingCapacity[node.v];
            while (k > 0 && capacity > 0) {
                System.out.print(node.cost + f + " ");
                k--;
                capacity--;
            }

            if (k == 0) break;

            visited[node.v] = true;

            adjList[node.v].forEach(i -> {
                if (!visited[i.v]) {
                    queue.add(new Node(i.v, i.w, node.cost + i.w));
                }
            });
        }

        while (k > 0) {
            System.out.print("-1 ");
            k--;
        }
    }
}
