import java.util.Scanner;

public class AccurateMovement {
    public static void main(String[] args) {
        float a, b, n;
        Scanner in = new Scanner(System.in);
        a = in.nextInt();
        b = in.nextInt();
        n = in.nextInt();
        long ans = 2 * Math.round(Math.ceil((n - b) / (b - a))) + 1;
        System.out.println(ans);
    }
}
