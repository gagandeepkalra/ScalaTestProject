import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/*
Compute average of stock prices, multi-threaded implementation
 */
public class Stats {

    public static class StatisticsAggregatorImpl implements StatisticsAggregator {
        private Map<String, Pair> hashMap = new ConcurrentHashMap<>();

        @Override
        public void putNewPrice(String symbol, double price) {
            hashMap.putIfAbsent(symbol, new Pair());
            hashMap.get(symbol).update(price);
        }

        @Override
        public double getAveragePrice(String symbol) {
            return hashMap.getOrDefault(symbol, new Pair()).average();
        }

        @Override
        public int getTickCount(String symbol) {
            return hashMap.getOrDefault(symbol, new Pair()).getCount();
        }

        private static class Pair {
            private double total = 0.0;
            private int count = 0;

            public int getCount() {
                return count;
            }

            synchronized void update(double value) {
                total += value;
                count++;
            }

            Double average() {
                if (count == 0) return 0.0;
                else return total / count;
            }
        }
    }

    ////////////////// DO NOT MODIFY BELOW THIS LINE ///////////////////

    public interface StatisticsAggregator {
        // This is an input. Make note of this price.
        public void putNewPrice(String symbol, double price);

        // Get the average price
        public double getAveragePrice(String symbol);

        // Get the total number of prices recorded
        public int getTickCount(String symbol);
    }

    public static void main(String[] args) {

        Scanner scanner = new Scanner(System.in);
        while (scanner.hasNext()) {
            final StatisticsAggregator stats = new StatisticsAggregatorImpl();
            final Set<String> symbols = new TreeSet<>();

            String line = scanner.nextLine();
            String[] inputs = line.split(",");
            int threads = Integer.parseInt(inputs[0]);
            ExecutorService pool = Executors.newFixedThreadPool(threads);
            for (int i = 1; i < inputs.length; ++i) {
                String[] tokens = inputs[i].split(" ");
                final String symbol = tokens[0];
                symbols.add(symbol);
                final double price = Double.parseDouble(tokens[1]);
                pool.submit(new Runnable() {
                    @Override
                    public void run() {
                        stats.putNewPrice(symbol, price);
                    }
                });

            }
            pool.shutdown();
            try {
                pool.awaitTermination(5000, TimeUnit.MILLISECONDS);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            for (String symbol : symbols) {
                System.out.println(String.format("%s %.4f %d", symbol,
                        stats.getAveragePrice(symbol),
                        stats.getTickCount(symbol)));
            }
        }
        scanner.close();

    }
}
