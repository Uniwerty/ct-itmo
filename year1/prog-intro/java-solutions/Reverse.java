import java.util.Scanner;
import java.lang.Math;
import java.io.IOException;

public class Reverse {
    public static void main(final String[] args) {
        MyScanner sc = new MyScanner(System.in);
        try {
            int[][] numbers = new int[10][];
            int i = 0;
            while (sc.hasNextLine()) {
                if (numbers.length <= i) {
                    int[][] newNumbers = new int[numbers.length * 2][];
                    for (int j = 0; j < numbers.length; j++) {
                        newNumbers[j] = new int[numbers[j].length];
                        for (int k = 0; k < numbers[j].length; k++) {
                            newNumbers[j][k] = numbers[j][k];
                        }
                    }
                    numbers = newNumbers;
                }
                String str = sc.nextLine();
                MyScanner lenStr = new MyScanner(str);
                int l = 0;
                while (lenStr.hasNext()) {
                    lenStr.next();
                    l++;
                }
                numbers[i] = new int[l];
                MyScanner fromStr = new MyScanner(str);
                for (int j = 0; j < l; j++) {
                    numbers[i][j] = Integer.parseInt(fromStr.next());
                }
                i++;
            }
            for (int k = i - 1; k >= 0; k--) {
                for (int j = numbers[k].length - 1; j >= 0; j--) {
                    System.out.print(numbers[k][j] + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
              System.out.println(e.getMessage());
        }
    }
}