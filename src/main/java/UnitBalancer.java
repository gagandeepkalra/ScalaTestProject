import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.StringTokenizer;


/*

Unit Balancer
Write a program to sort a set of given units and output a single relationship equation among the units in descending order of size. The input given will be a series of comma separated units and a set of relationship equations between them. From these equations, you are expected to derive a single relationship equation in descending order of the units, with the largest unit on the left. Further, the following are given:

The number of equations given will be 1 less than the number of units given

To keep it simple, only units that can be expressed as integer multiples of each other should be considered. Meaning, the equations must not contain fractional multipliers

Input
First line contains name of all the units separated by comma - no spaces
If there are  units in the above line then there will be  lines in the input that defines relation between the quanitites. The input format of the relationship between the units is -
 where  is the string that denotes unit on the left hand side of the equation , then followed by the space is the = symbol , then followed by space is an integer value  and then followed by space is the string that denotes unit on the right hand side.

Output

In the output you need to print a single string that denotes relation between all the units in the descending order of their value as per  the sample output.

Constraints

Sample Input
day,hour,second,minute
day = 24 hour
hour = 60 minute
minute = 60 second

 */
public class UnitBalancer {

    private static class Relation {
        long times;
        String token;

        public Relation(long times, String token) {
            this.times = times;
            this.token = token;
        }
    }

    private static String equivalence(Map<String, Relation> map, long prefixMultiplier, Relation relation) {
        if (relation == null) return "";
        return " = " + (prefixMultiplier * relation.times) + relation.token + equivalence(map, (prefixMultiplier * relation.times), map.get(relation.token));
    }

    public static void main(String args[]) throws Exception {
        BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine(), ",");

        Map<String, List<Relation>> map = new HashMap<>();
        int m = 0;
        while (st.hasMoreTokens()) {
            map.put(st.nextToken(), new ArrayList<>());
            m++;
        }


        while (m-- > 1) {
            st = new StringTokenizer(br.readLine());
            String first = st.nextToken();
            st.nextToken();
            int times = Integer.parseInt(st.nextToken());
            String second = st.nextToken();

            List<Relation> neighborsFirst = map.get(first);
            neighborsFirst.add(new Relation(times, second));
            map.put(first, neighborsFirst);

            if (times == 1) {
                List<Relation> neighborsSecond = map.get(second);
                neighborsSecond.add(new Relation(1, first));
                map.put(second, neighborsSecond);
            }
        }

        for (String key : map.keySet()) {
            PriorityQueue<Relation> result = abc(map, key);

            if (result.size() == map.keySet().size()) {
                System.out.print("1" + result.poll().token);
                while (!result.isEmpty()) {
                    Relation relation = result.poll();
                    System.out.print(" = " + relation.times + relation.token);
                }
                return;
            }
        }
    }

    static PriorityQueue abc(Map<String, List<Relation>> map, String maxToken) {
        Relation first = new Relation(1, maxToken);

        Queue<Relation> queue = new LinkedList<>();
        queue.add(first);

        PriorityQueue<Relation> result = new PriorityQueue<>((o1, o2) -> {
            long diff = (o1.times - o2.times);
            if (diff > 0) return 1;
            else if (diff < 0) return -1;
            else return 0;
        });

        Set<String> visited = new HashSet<>();

        while (!queue.isEmpty()) {
            Relation relation = queue.poll();
            if (visited.contains(relation.token)) continue;

            visited.add(relation.token);
            result.add(relation);

            List<Relation> neighbors = map.get(relation.token);

            neighbors.stream().filter(neighborRelation -> !visited.contains(neighborRelation.token)).forEach(neighborRelation -> {
                if (!visited.contains(neighborRelation.token)) {
                    queue.add(new Relation(relation.times * neighborRelation.times, neighborRelation.token));
                }
            });
        }

        return result;
    }
}
