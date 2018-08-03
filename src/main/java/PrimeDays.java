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
     * @param lot        2-D matrix with 3 values, 0 -> obstacle, 1-> available, 9 -> destination
     * @return minimum length of path to reach destination starting from (0, 0)
     */
    static int removeObstacle(int numRows, int numColumns, List<List<Integer>> lot) {
        boolean[][] visited = new boolean[numRows][numColumns];

        PriorityQueue<List<Integer>> queue = new PriorityQueue<>(Comparator.comparingInt(o -> o.get(2)));
        queue.add(Arrays.asList(0, 0, 1)); // (r, c, d)

        int[] dx = {-1, 0, 1, 0};
        int[] dy = {0, 1, 0, -1};

        while (!queue.isEmpty()) {
            List<Integer> val = queue.poll();
            int r = val.get(0), c = val.get(1), d = val.get(2);

            if (lot.get(r).get(c) == 9) return d;
            else if (!visited[r][c]) {
                visited[r][c] = true;
                for (int i = 0; i < 4; i++) {
                    int newR = r + dx[i], newC = c + dy[i];
                    if (0 <= newR && newR < numRows && 0 <= newC && newC < numColumns &&
                            !visited[newR][newC] && lot.get(newR).get(newC) != 0)
                        queue.add(Arrays.asList(newR, newC, d + 1));
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
