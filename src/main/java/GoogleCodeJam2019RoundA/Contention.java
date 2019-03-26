package GoogleCodeJam2019RoundA;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class Contention {

    static class Pair {
        int l, r;

        public Pair(int l, int r) {
            this.l = l;
            this.r = r;
        }
    }

    public static void main(String[] args) {
        final Scanner sc = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
        int cases = sc.nextInt();

        for (int t = 1; t <= cases; t++) {
            int n = sc.nextInt(), q = sc.nextInt();

            Pair[] pairs = new Pair[q];
            for (int i = 0; i < q; i++) {
                pairs[i] = new Pair(sc.nextInt(), sc.nextInt());
            }
            Arrays.sort(pairs, Comparator.comparingInt(p -> (p.r - p.l)));

            LinkedList<Pair> ls = new LinkedList<>();
            ls.add(new Pair(1, n));

            int result = Integer.MAX_VALUE;
            for (Pair pair : pairs) {
                result = Math.min(result, adjust(ls, pair));
            }

            System.out.println(String.format("Case #%d: %d", t, result));
        }
    }

    private static int adjust(LinkedList<Pair> available, Pair toAdd) {
        int result = 0;
        for (ListIterator<Pair> itr = available.listIterator(); itr.hasNext(); ) {
            Pair pair = itr.next();

            if (pair.r < toAdd.l || toAdd.r < pair.l) {
                continue;
            }

            if (pair.l <= toAdd.l && toAdd.r <= pair.r) { // pair is big, fully enclosed, divide it
                itr.remove();
                if (pair.l < toAdd.l) {
                    itr.add(new Pair(pair.l, toAdd.l - 1));
                }
                if (toAdd.r < pair.r) {
                    itr.add(new Pair(toAdd.r + 1, pair.r));
                }
                return toAdd.r - toAdd.l + 1;

            } else if (toAdd.l <= pair.l && pair.r <= toAdd.r) { // toAdd is big, delete pair
                itr.remove();
                result += pair.r - pair.l + 1;

            } else if (toAdd.l < pair.l) {
                itr.remove();
                if (toAdd.r < pair.r) {
                    itr.add(new Pair(toAdd.r + 1, pair.r));
                }

                result += toAdd.r - pair.l + 1;

            } else if (pair.l < toAdd.l) {
                itr.remove();
                itr.add(new Pair(pair.l, toAdd.l - 1));

                result += pair.r - toAdd.l + 1;
            }
        }

        return result;
    }
}
