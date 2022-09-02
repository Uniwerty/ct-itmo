package expression;

import expression.generic.*;

public class Main {
    public static void main(String[] args) {
        GenericTabulator tabulator = new GenericTabulator();
        int x1 = -2, y1 = -2, z1 = -2;
        int x2 = 2, y2 = 2, z2 = 2;
        try {
            Object[][][] table = tabulator.tabulate(args[0].substring(1), args[1], x1, x2, y1, y2, z1, z2);
            for (int i = 0; i <= x2 - x1; i++) {
                for (int j = 0; j <= y2 - y1; j++) {
                    for (int k = 0; k <= z2 - z1; k++) {
                        System.out.println("x=" + (x1 + i) + " y=" + (y1 + j) + " z=" + (z1 + k) + ": " + table[i][j][k]);
                    }
                }
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}