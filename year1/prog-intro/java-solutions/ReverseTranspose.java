import java.util.Scanner;
import java.lang.Math;
import java.io.IOException;

public class ReverseTranspose {
    public static void main(final String[] args) {
	    Scanner sc = new Scanner(System.in);
	    int[][] numbers = new int[10][];
	    int i = 0;
	    int maxLen = 0;
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
	        Scanner lenStr = new Scanner(str);
	        int l = 0;
	        while (lenStr.hasNext()) {
	            lenStr.next();
		    l++;
	        }
	        numbers[i] = new int[l];
	        maxLen = Math.max(maxLen, l);
	        Scanner fromStr = new Scanner(str);
	        for (int j = 0; j < l; j++) numbers[i][j] = Integer.parseInt(fromStr.next());
	        i++;
	    }
	    for (int k = 0; k < maxLen; k++) {
	        for (int j = 0; j < i; j++) {
		    if (k < numbers[j].length) {
		        System.out.print(numbers[j][k] + " ");
		    }
	        }
	        System.out.println();
	    }
    }
}