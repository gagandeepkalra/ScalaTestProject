import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.stream.IntStream;

public class NiYOTest {

    static class Node {
        public char c;
        public int[] frequency = new int[26];

        public Node(char c) {
            this.c = c;
            frequency[c - 'a'] = 1;
        }

        public void add(Node node) {
            for (int i = 0; i < 26; i++) {
                frequency[i] += node.frequency[i];
            }
        }

    }

    static class Tree {
        public int V;
        public LinkedList<Integer> adjListArray[];

        public Tree(int V) {
            this.V = V;

            adjListArray = new LinkedList[V];

            for (int i = 0; i < V; i++) {
                adjListArray[i] = new LinkedList<>();
            }
        }

        public void addEdge(int src, int dest) {
            adjListArray[src].addFirst(dest);
            adjListArray[dest].addFirst(src);
        }
    }

    static void populate(Tree tree, Node[] nodes, int index, int parent) {
        tree.adjListArray[index].stream().filter(integer -> integer != parent).forEach(integer -> {
            populate(tree, nodes, integer, index);
            nodes[index].add(nodes[integer]);
        });
    }


    public static void main(String args[]) throws Exception{
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));

        StringTokenizer st = new StringTokenizer(br.readLine());


        int N = Integer.parseInt(st.nextToken()), Q = Integer.parseInt(st.nextToken());
        Node[] nodes = new Node[N + 1];

        st = new StringTokenizer(br.readLine());

        for (int i = 1; i <= N; i++) {
            nodes[i] = new Node(st.nextToken().charAt(0));
        }

        int t = N - 1;
        Tree tree = new Tree(N + 1);
        while (t-- > 0) {
            st=new StringTokenizer(br.readLine());
            tree.addEdge(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
        }

        populate(tree, nodes, 1, 0);

        while (Q-- > 0) {
            st = new StringTokenizer(br.readLine());
            Node node = nodes[Integer.parseInt(st.nextToken())];
            int[] frequency = new int[26];

            st.nextToken().chars().forEach(c -> frequency[c - 'a']++);

            System.out.println(IntStream.range(0, 26).map(i -> {
                if (frequency[i] > node.frequency[i]) return frequency[i] - node.frequency[i];
                else return 0;
            }).sum());

        }
    }

}
