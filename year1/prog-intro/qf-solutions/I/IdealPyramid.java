import java.util.Scanner;

public class IdealPyramid {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        long xl = Long.MAX_VALUE, xr = Long.MIN_VALUE, yl = Long.MAX_VALUE, yr = Long.MIN_VALUE;
        for (int i = 0; i < n; i++) {
            long xi = in.nextInt();
            long yi = in.nextInt();
            long hi = in.nextInt();
            xl = Math.min(xl, xi - hi);
            xr = Math.max(xr, xi + hi);
            yl = Math.min(yl, yi - hi);
            yr = Math.max(yr, yi + hi);
        }
        long h = Math.round(Math.ceil(Math.max(xr - xl, yr - yl) / 2.0));
        long x = (xl + xr) / 2;
        long y = (yl + yr) / 2;
        System.out.println(x + " " + y + " " + h);
    }
}
