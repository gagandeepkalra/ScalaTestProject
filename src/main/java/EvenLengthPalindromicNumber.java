import java.util.Scanner;

public class EvenLengthPalindromicNumber {
    public static void main(String[] args) {
        final Scanner sc = new Scanner(System.in);
        int t = sc.nextInt();
        while (t-- > 0) {
            int[] frequency = new int[10];
            int max = Integer.MIN_VALUE;
            for (char c : sc.next().toCharArray()) {
                frequency[c - '0']++;
                max = Math.max(max, frequency[c - '0']);
            }
            for (int i = 0; i < 10; i++) {
                if (frequency[i] == max) {
                    System.out.println(i);
                    break;
                }
            }

        }
    }
}
