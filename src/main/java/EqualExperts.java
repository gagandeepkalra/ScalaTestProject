import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;
import java.util.StringTokenizer;

public class EqualExperts {

    // Q1
    static class MyDS {
        int size = 0;
        int max = Integer.MIN_VALUE;
        HashMap<Integer, Integer> hash; // val -> key, key

        MyDS() {
            hash = new HashMap<>();
        }

        void add(int x) {
            Integer temp = hash.get(x);

            if (temp != null && temp == 2) return;

            if (x >= max) {
                max = x;
                if (temp == null) {
                    hash.put(x, 1);
                    size++;
                }
                return;
            }
            size++;
            if (temp == null) hash.put(x, 1);
            else hash.put(x, temp + 1);
        }

        void printBSequence() {
            Set<Integer> set = hash.keySet();
            int[] sequence = new int[set.size()];

            Iterator<Integer> iterator = set.iterator();
            for (int i = 0; i < set.size(); i++) {
                sequence[i] = iterator.next();
            }

            Arrays.sort(sequence);

            for (int aSequence : sequence) {
                Integer temp = hash.get(aSequence);
                hash.put(aSequence, temp - 1);
                System.out.print(aSequence + " ");
            }

            for (int i = sequence.length - 1; i >= 0; i--) {
                if (hash.get(sequence[i]) != 0) {
                    System.out.print(sequence[i] + " ");
                }
            }
        }
    }

    // Increasing Decreasing sequence with each element repeating twice, once in each sequence,
    // for each query check if it can be inserted and in the end print the modified sequence
    public static void Q1(String args[]) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));

        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());

        MyDS store = new MyDS();

        st = new StringTokenizer(br.readLine());
        for (int i = 0; i < n; i++) {
            store.add(Integer.parseInt(st.nextToken()));
        }

        st = new StringTokenizer(br.readLine());
        int q = Integer.parseInt(st.nextToken());

        while (q-- > 0) {
            st = new StringTokenizer(br.readLine());
            store.add(Integer.parseInt(st.nextToken()));

            System.out.println(store.size);
        }
        store.printBSequence();
    }


    // Q2
    static int[] values = new int[100001];

    static class Tree {
        public int V;
        public ArrayList<Integer> adjListArray[];

        public Tree(int V) {
            this.V = V;

            adjListArray = new ArrayList[V];

            for (int i = 0; i < V; i++) {
                adjListArray[i] = new ArrayList();
            }
        }

        public void addEdge(int src, int dest) {
            adjListArray[src].add(dest);
            adjListArray[dest].add(src);
        }

        int BFS(int node, long k) {
            // Create a queue for BFS
            LinkedList<Element> queue = new LinkedList<Element>();

            boolean[] visited = new boolean[V];

            queue.add(new Element(node, 1, values[node]));
            visited[node] = true;

            while (queue.size() != 0) {
                Element element = queue.remove();

                if (element.value >= k) {
                    return element.depth; //result
                }

                adjListArray[element.node].stream()
                        .filter(integer -> !visited[integer])
                        .forEach(integer -> {
                            queue.add(new Element(integer, element.depth + 1, element.value + values[integer]));
                            visited[integer] = true;
                        });
            }

            return -1;
        }
    }

    static class Element {
        int node;
        int depth;
        long value;

        public Element(int node, int depth, long value) {
            this.node = node;
            this.depth = depth;
            this.value = value;
        }
    }

    // Tree, with values at each node, find minimum node to count to reach a certain value k, starting from any given node
    public static void Q2(String args[]) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));

        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());
        int q = Integer.parseInt(st.nextToken());

        Tree tree = new Tree(n + 1);


        st = new StringTokenizer(br.readLine());
        for (int i = 1; i <= n; i++) {
            values[i] = Integer.parseInt(st.nextToken());
        }

        n--;
        while (n-- > 0) {
            st = new StringTokenizer(br.readLine());
            tree.addEdge(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
        }

        while (q-- > 0) {
            st = new StringTokenizer(br.readLine());
            int node = Integer.parseInt(st.nextToken());
            long k = Long.parseLong(st.nextToken());
            System.out.println(tree.BFS(node, k));
        }


    }
}
