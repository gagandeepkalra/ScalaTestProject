import java.util.Scanner;

/*

Candies
You are given a string S consisting of lowercase English letters denoting different types of candies. A substring of a
string S is a string S' that occurs in S. For example, "bam" is a substring of "babammm". Each candy costs 1 unit. You
can pick some continuous candies such that you can create a palindrome of length K by using some or all picked candies.
Your task is to find the minimum cost to create a palindrome of length K.

Input Format:
First line contains string S.
Next line contains an integer T denoting the number of test cases.
Next T lines contain a single integer K.

Output Format:
For each test case, print minimum cost as mentioned above. If you cannot create a palindrome of length K then, simply print -1.

Sample Input
babammm
2
2
5
Sample Output
2
5

Explanation
Test Case 1: You can pick candies denoted by "mm" and create a palindrome of size 2. So the cost will be 2 units.
Test Case 2: You can pick candies denoted by "babam" and rearrange them, "bamab", to create a palindrome of size 5. So the cost will be 5 units.

 */
public class SlidingWindowPalindrome {

    private static int countPalindromicPairs(int[] frequencyMap) {
        int count = 0;
        for (int i = 0; i < 26; i++) {
            count += frequencyMap[i] / 2;
        }
        return count;
    }

    public static void main(String[] args) {
        final Scanner sc = new Scanner(System.in);
        final String inputString = sc.next();

        int t = sc.nextInt();
        while (t-- > 0) {
            int k = sc.nextInt();
            int[] frequencyMap = new int[26];

            int i = 0, j = 0;
            int minCost = Integer.MAX_VALUE;

            boolean wasKOdd = false;
            if (k % 2 != 0) {
                k--;
                wasKOdd = true;
            }

            if (k == 0) {
                System.out.println(1);
                continue;
            }

            while (j < inputString.length()) {

                while (j < inputString.length() && countPalindromicPairs(frequencyMap) * 2 != k) {
                    frequencyMap[inputString.charAt(j) - 'a']++;
                    j++;
                }

                while (countPalindromicPairs(frequencyMap) * 2 == k) {
                    frequencyMap[inputString.charAt(i) - 'a']--;
                    i++;
                }

                minCost = Math.min(minCost, j - i + 1);
            }

            if (i == 0) {
                System.out.println(-1);
            } else if (wasKOdd && minCost == k) {
                System.out.println(minCost + 1);
            } else {
                System.out.println(minCost);
            }
        }
    }
}
