import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;

public class Median {

    /*
     * Complete the 'median' function below.
     *
     * The function is expected to return a DOUBLE.
     * The function accepts DOUBLE_ARRAY array as parameter.
     */
    public static double median(List<Double> list) {
        Double[] numArray = list.stream().filter(aDouble -> aDouble >= 0).toArray(Double[]::new);
        Arrays.sort(numArray);
        if (numArray.length % 2 == 0)
            return (numArray[numArray.length / 2] + numArray[numArray.length / 2 - 1]) / 2;
        else
            return numArray[numArray.length / 2];

    }

    public static void main(String[] args) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));

        int arrayCount = Integer.parseInt(bufferedReader.readLine().trim());

        List<Double> array = IntStream.range(0, arrayCount).mapToObj(i -> {
            try {
                return bufferedReader.readLine().replaceAll("\\s+$", "");
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            }
        }).map(String::trim).map(Double::parseDouble).collect(toList());

        double result = median(array);
        System.out.println(result);
    }
}
