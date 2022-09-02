import java.util.Scanner;
import java.io.*;

public class ReverseHex2 {
    public static void main(final String[] args) {
	try {
	    MyScanner sc = new MyScanner(System.in);
	    long[][] numbers = new long[10][];
	    int i = 0;
	    while (sc.hasNextLine()) {
	        if (numbers.length <= i) {
		    long[][] newNumbers = new long[numbers.length * 2][];
		    for (int j = 0; j < numbers.length; j++) {
		        newNumbers[j] = new long[numbers[j].length]; 
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
	        lenStr.close();
	        numbers[i] = new long[l];
	        MyScanner fromStr = new MyScanner(str);
	        for (int j = 0; j < l; j++) {
		    String number = fromStr.next();
		    numbers[i][j] = Integer.parseUnsignedInt(number, 16);
	        }
	        fromStr.close();
	        i++;
	    }
	    sc.close();
	    for (int j = i - 1; j >= 0; j--) {
	        for (int k = numbers[j].length - 1; k >= 0; k--) {
		    System.out.print(numbers[j][k] + " ");
	        }
	    System.out.println();
	    }
	} catch (IOException e) {
	    System.out.println("Scanner error: " + e.getMessage());
	}
    }
}
