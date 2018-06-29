import java.util.Scanner;

/*

Poisonous gas
You are in a battle field and your enemy has an army of N soldiers. Each soldier has a strength denoted by an array A.
Your enemy will select some soldiers such that total strength of selected soldiers is maximum. You have a poisonous gas
and you can use it on the selected soldiers as many times as you want. If the total strength is even, the poisonous gas
will decrease the total strength of the selected soldiers to half of the total strength, otherwise it will not affect
them. Your task is to tell if its possible to reduce the total strength to 1 or not.



Input Format:
First line contains an integer T, denoting the number of test cases.
First line of each test case contains an integer N, denoting the number of soldiers.
Second line of each test case contains N space-separated integers, denoting the strength of the soldiers.

Output Format:
For each test case, print Yes if its possible to reduce the total strength to 1, otherwise print No.

Sample Input
2
5
76 56 -21 76 -45
2
8 -4
Sample Output
No
Yes

Explanation
Test case 1: n = 5 and maximum total strength is 76 + 56 + 76 = 208. You can not reduce 208 to 1 by using the poisonous
gas.

Test case 2: n = 2 and maximum total strength is 8. First time when you will use the poisonous gas, total strength will
reduce to 4. Second time when you will use the poisonous gas, total strength will reduce to 2. Third time when you will
use the poisonous gas, total strength will reduce to 1. Therefore, the answer is Yes

 */
public class PoisonousGas {
    public static void main(String args[]) {
        Scanner sc = new Scanner(System.in);
        int t = sc.nextInt();

        for (int i = 0; i < t; i++) {

            int n = sc.nextInt();
            long a[] = new long[n];
            long strength = 0;

            for (int j = 0; j < n; j++) {
                a[j] = sc.nextLong();
                if (a[j] > 0) strength += a[j];
            }

            while (strength > 1 && strength % 2 == 0) {
                strength = strength / 2;
            }

            if (strength == 1) {
                System.out.println("Yes");
            } else {
                System.out.println("No");
            }
        }
    }
}

