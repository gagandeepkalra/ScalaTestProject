package expedia;

import java.util.Scanner;

public class CountTheSolutions {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        final long s = sc.nextLong();

        long counter = 0;

        for (long d = 0; d <= 1000; d++) {
            long sumWithD = d * d * d * d;

            if (sumWithD > s) break;
            else if (s - sumWithD > 1001001000) counter += 1003003001;
            else {
                for (long c = 0; c <= 1000; c++) {
                    long sumWithCD = c * c * c + sumWithD;

                    if (sumWithCD > s) break;
                    else if (s - sumWithCD > 1001000) counter += 1002001;
                    else {

                        for (long b = 0; b <= 1000; b++) {

                            long sumWithBCD = b * b + sumWithCD;

                            if (sumWithBCD > s) break;
                            else if (s - sumWithBCD > 1000) counter += 1001;
                            else {
                                counter += s - sumWithBCD + 1;
                            }
                        }

                    }
                }
            }

        }

        System.out.println(counter);
    }
}
