import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.stream.IntStream;

/*

Dial Maximum
Consider a dial pad of 1 to 9 digits as shown in the figure. You can not press a single digit 2 times consecutively. If you are pressing any digit, then after pressing that digit, you can only press its adjacent digit. Two digits are adjacent if they share an edge.
Cost of pressing a digit after another adjacent digit is given. Initially, you have X unit(s) of money. You need to maximize the sum of digit(s) you can press in X unit(s) of money. You can start from any digit.

Input :

The first line of input contains an integer X, denoting amount of money you have.
Each of the following 12 lines contains u, v and w, where w is the cost of changing digit from u to v or v to u.
Output :

A single integer representing the maximum sum of numbers you can press in X unit of money.

Sample Input
15
1 2 1
2 3 1
4 5 1
5 6 1
7 8 1
8 9 1
1 4 1
2 5 1
3 6 1
4 7 1
5 8 1
6 9 1
Sample Output
136
Explanation
we can follow given sequence : [9,8,9,8,9,8,9,8,9,8,9,8,9,8,9,8]

 */
public class DialMaximum {

    static class Node {
        int v;
        int cost;

        Node(int v, int w) {
            this.v = v;
            this.cost = w;
        }
    }

    static class Graph {
        int V; // no. of vertices
        LinkedList<Node> neighbours[];

        Graph(int V) {
            this.V = V;

            neighbours = new LinkedList[V];

            for (int i = 0; i < V; i++) {
                neighbours[i] = new LinkedList<>();
            }
        }

        void addEdge(int src, int dest, int weight) {
            neighbours[src].addFirst(new Node(dest, weight));
            neighbours[dest].addFirst(new Node(src, weight));
        }

        public class Pair<L, R> {

            private final L first;
            private final R second;

            public Pair(L first, R second) {
                this.first = first;
                this.second = second;
            }

            public L getFirst() {
                return first;
            }

            public R getSecond() {
                return second;
            }

            @Override
            public int hashCode() {
                return first.hashCode() ^ second.hashCode();
            }

            @Override
            public boolean equals(Object o) {
                if (!(o instanceof Pair)) return false;
                Pair pairo = (Pair) o;
                return this.first.equals(pairo.getFirst()) &&
                        this.second.equals(pairo.getSecond());
            }

        }

        private Map<Pair, Integer> map = new HashMap<>();

        int getMaximumSum(int start, int money) {
            Pair pair = new Pair<>(start, money);
            if (map.containsKey(pair)) {
                return map.get(pair);
            } else {
                int result = neighbours[start].stream()
                        .filter(node -> money >= node.cost)
                        .map(node -> getMaximumSum(node.v, money - node.cost))
                        .max(Integer::compareTo)
                        .orElse(0) + start;

                map.put(pair, result);
                return result;
            }
        }
    }


    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int x = Integer.parseInt(st.nextToken());

        Graph graph = new Graph(10);

        for (int i = 0; i < 12; i++) {
            st = new StringTokenizer(br.readLine());
            graph.addEdge(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
        }

        int[][] dp = new int[10][x + 1];

        for (int j = 0; j <= x; j++) {
            for (int i = 1; i <= 9; i++) {
                dp[i][j] += i;
                for (Node node : graph.neighbours[i]) {
                    if (j + node.cost <= x) {
                        dp[node.v][j + node.cost] = Math.max(dp[node.v][j + node.cost], dp[i][j]);
                    }
                }
            }
        }

        System.out.println(IntStream.range(1, 10).mapToObj(i -> dp[i][x]).max(Integer::compareTo).orElse(0));
    }
}
