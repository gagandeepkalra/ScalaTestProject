import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.StringTokenizer;

public class PerpendicularFiles {
    public static void main(String[] args) throws Exception {
        final BufferedReader br = new BufferedReader(
                new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());
        int t = Integer.parseInt(st.nextToken());

        while (t-- > 0) {
            st = new StringTokenizer(br.readLine());
            BigInteger x1 = new BigInteger(st.nextToken()), y1 = new BigInteger(st.nextToken()),
                    x2 = new BigInteger(st.nextToken()), y2 = new BigInteger(st.nextToken());

            st = new StringTokenizer(br.readLine());
            BigInteger x3 = new BigInteger(st.nextToken()), y3 = new BigInteger(st.nextToken()),
                    x4 = new BigInteger(st.nextToken()), y4 = new BigInteger(st.nextToken());

            if (x2.equals(x1) && y1.equals(y2) || x3.equals(x4) && y3.equals(y4)) {
                System.out.println("INVALID");
            } else {
                BigInteger num = (y2.subtract(y1)).multiply((y3.subtract(y4)));
                BigInteger deno = (x2.subtract(x1)).multiply((x3.subtract(x4)));

                if (num.equals(deno.multiply(new BigInteger("-1")))) System.out.println("YES");
                else System.out.println("NO");
            }
        }
    }
}
