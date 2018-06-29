import java.util.Arrays;
import java.util.PriorityQueue;
import java.util.Scanner;

public class ThoughtWorks {

    // Q1

    static long findX(long n) {
        return (long) (Math.pow(2, Math.floor(Math.log(n) / Math.log(2))));
    }

    public static void input1(String[] args) {
        Scanner sc = new Scanner(System.in);
        int t = sc.nextInt();
        for (int i = 0; i < t; i++) {
            long n = sc.nextLong();
            long x = findX(n);

            long count = n - x;
            if (count == 0) {
                System.out.println(n);
            } else {
                System.out.println(2 + (count - 1) * 2);
            }
        }
    }

    // Q2- event based sorting, find max overlapping events

    static class Time {
        private int hours;
        private int mins;

        public Time(int hours, int mins, boolean flag) {
            this.hours = hours;
            this.mins = mins;
            if (flag) add(5);
        }

        private void add(int value) {
            mins += value;
            if (mins >= 60) {
                hours += 1;
                mins = mins - 60;
            }
        }

        public double getValue() {
            return hours * 1.0 + ((mins * 1.0) / 100.0);
        }
    }

    static class Pair {
        Double start;
        Double end;

        public Pair(Double start, Double end) {
            this.start = start;
            this.end = end;
        }
    }

    public static void input2(String[] args) {
        Scanner sc = new Scanner(System.in);
        int t = sc.nextInt();
        Pair[] ranges = new Pair[t];
        for (int i = 0; i < t; i++) {
            ranges[i] = (new Pair(new Time(sc.nextInt(), sc.nextInt(), false).getValue(), new Time(sc.nextInt(), sc.nextInt(), true).getValue()));
        }

        Arrays.sort(ranges, (Pair o1, Pair o2) -> {
            int result = ((int) Math.signum(o1.start - o2.start));
            if (result == 0) {
                return ((int) Math.signum(o1.end - o2.end));
            } else return result;
        });


        PriorityQueue<Double> queue = new PriorityQueue<>();
        int count = 0;
        int result = Integer.MIN_VALUE;

        for (int i = 0; i < t; i++) {
            Pair pair = ranges[i];

            while (!queue.isEmpty() && queue.peek() <= pair.start) {
                queue.remove();
                count--;
            }

            queue.add(pair.end);
            count++;

            result = Math.max(result, count);
        }
        System.out.println(result);
    }
}

    /*
    4
00 00 11 11
00 00 10 11
11 17 23 17
12 12 12 50
     */


