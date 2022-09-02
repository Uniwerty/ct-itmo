import java.util.Scanner;

public class BadTreap {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int cnt = 0;
        for (int k = -25000; k <= 25000; k++) {
            if (cnt < n) System.out.println(710*k);
            else break;
            cnt++;
        }
    }
}
