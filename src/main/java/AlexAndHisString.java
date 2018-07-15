import java.io.BufferedReader;
import java.io.InputStreamReader;

/*
Alex has a string S of length N consisting of lowercase alphabets. He wants to find lexicographically smallest string X of length N that can be formed using the following operation.

In one operation, he can select any one character among the at most first K characters of string S, remove it from string S and append it to string X. He can apply this operation as many times as he wants.

Help Alex find the string X.
 */
public class AlexAndHisString {

    private static void printMin(int[] freq) {
        for (int i = 0; i < 26; i++) {
            if (freq[i] > 0) {
                freq[i]--;
                System.out.print((char) ('a' + i));
                return;
            }
        }
    }

    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));

        final String str = br.readLine();
        int k = Integer.parseInt(br.readLine());

        int[] freq = new int[26];

        for (int i = 0; i < k; i++) freq[str.charAt(i) - 'a']++;

        for (int i = k; i < str.length(); i++) {
            printMin(freq);
            freq[str.charAt(i) - 'a']++;
        }
        while (k-- > 0) {
            printMin(freq);
        }
        System.out.println();
    }
}
