import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.stream.Collectors;

public class PrimeDays {

    /**
     * Dijkstra
     *
     * @param numRows
     * @param numColumns
     * @param area       2-D matrix with 3 values, 0 -> obstacle, 1-> available, 9 -> destination
     * @return minimum length of path to reach destination starting from (0, 0)
     */
    static int minimumDistance(int numRows, int numColumns, List<List<Integer>> area) {
        boolean[][] visitedMatrix = new boolean[numRows][numColumns];

        PriorityQueue<List<Integer>> priorityQueue = new PriorityQueue<>(Comparator.comparingInt(o -> o.get(2)));
        priorityQueue.add(Arrays.asList(0, 0, 1)); // (r, c, d)

        int[] deltaX = {-1, 0, 1, 0};
        int[] deltaY = {0, 1, 0, -1};

        while (!priorityQueue.isEmpty()) {
            List<Integer> head = priorityQueue.poll();
            int r = head.get(0), c = head.get(1), d = head.get(2);

            if (area.get(r).get(c) == 9) return d;
            else if (!visitedMatrix[r][c]) {
                visitedMatrix[r][c] = true;
                for (int i = 0; i < 4; i++) {
                    int newR = r + deltaX[i], newC = c + deltaY[i];
                    if (0 <= newR && newR < numRows && 0 <= newC && newC < numColumns && !visitedMatrix[newR][newC] && area.get(newR).get(newC) != 0)
                        priorityQueue.add(Arrays.asList(newR, newC, d + 1));
                }
            }
        }
        return -1;
    }

    /**
     * Nearest points from (0, 0) in 2-D plane
     *
     * @param totalSteakhouses
     * @param allLocations     list of (x, y) coordinates
     * @param numSteakhouses
     * @return nearest #numSteakhouses from (0, 0)
     */
    static List<List<Integer>> nearestXsteakHouses(int totalSteakhouses,
                                                   List<List<Integer>> allLocations,
                                                   int numSteakhouses) {
        return allLocations.stream()
                .sorted(Comparator.comparingInt(ls -> ls.get(0) * ls.get(0) + ls.get(1) * ls.get(1)))
                .limit(numSteakhouses)
                .collect(Collectors.toList());
    }
}
