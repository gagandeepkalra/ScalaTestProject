import java.util.*;
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

    /**
     * Sort the list into old and new members based on if values are numerical(new),
     * Sort the old members, then append the new ones in there original order.
     *
     * @param numberOfBoxes
     * @param boxList
     * @return
     */
    static List<String> orderedJunctionBoxes(int numberOfBoxes, List<String> boxList) {
        List<List<String>> oldList = new ArrayList<>();
        List<String> newList = new ArrayList<>();
        boxList.forEach(str -> {
            String[] strArray = str.split(" ");

            boolean isNew = true;
            for (int i = 1; i < strArray.length; i++) isNew = isNew & strArray[i].matches("^[0-9]*$");

            if (isNew)
                newList.add(str);
            else
                oldList.add(Arrays.asList(strArray[0], str.replaceFirst(strArray[0], "")));
        });

        oldList.sort((o1, o2) -> {
            int compareResult = o1.get(1).compareTo(o2.get(1));
            if (compareResult == 0) return o1.get(0).compareTo(o2.get(0));
            else return compareResult;
        });

        List<String> resultList = oldList.stream().map(ls -> ls.get(0) + " " + ls.get(1)).collect(Collectors.toList());
        resultList.addAll(newList);
        return resultList;
    }
}
