import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.StringTokenizer;

/*
https://www.hackerearth.com/practice/data-structures/queues/basics-of-queues/practice-problems/algorithm/chocolate-distribution-3-f9297a6e/
 */
public class ChocolateDistribution {

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int t = Integer.parseInt(st.nextToken());

        PriorityQueue<Queue<Integer>> heap = new PriorityQueue<>(Comparator.comparingInt(Queue::peek));

        while (t-- > 0) {
            st = new StringTokenizer(br.readLine());
            int q = Integer.parseInt(st.nextToken()), k = Integer.parseInt(st.nextToken());

            while (q-- > 0) {
                st = new StringTokenizer(br.readLine());
                int n = Integer.parseInt(st.nextToken());

                st = new StringTokenizer(br.readLine());
                Queue<Integer> integerQueue = new LinkedList<>();
                while (n-- > 0) integerQueue.add(Integer.parseInt(st.nextToken()));

                heap.add(integerQueue);
            }

            long result = 0;

            while (k-- > 0) {
                if (heap.isEmpty()) break;

                Queue<Integer> queue = heap.remove();
                result += queue.remove();

                if (!queue.isEmpty()) heap.add(queue);
            }

            System.out.println(result);
        }
    }
}
