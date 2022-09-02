import java.util.Scanner;

public class JustTheLastDigit {
    public static void main(String[] args) {
        int[][] g = new int[500][500];
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        for (int i = 0; i < n; i++) {
            if (in.hasNext()) {
                String line = in.next();
                for (int j = 0; j < n; j++) {
                    g[i][j] = line.charAt(j) - '0';
                }
            }
        }
        for (int k = 0; k < n; k++) {
            for (int j = k + 1; j < n; j++) {
                if (g[k][j] == 1) {
                    for (int i = j + 1; i < n; i++) {
                        g[k][i] = ((g[k][i] - g[j][i]) % 10 + 10) % 10;
                    }
                }
            }
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                System.out.print(g[i][j]);
            }
            System.out.println();
        }
    }
}
