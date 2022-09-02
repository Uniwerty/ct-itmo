import java.lang.Character;
import java.util.Arrays;

public class SumFloat {
    public static void main(final String[] args) {
	int oper = 1;
        float sum = 0;
	for (int j = 0; j < args.length; j++) {
	    int i = 0;
	    while (i < args[j].length()) {
	        if (!Character.isWhitespace(args[j].charAt(i))) {
		    if (args[j].charAt(i) == '+') oper = 1;
		    else if (args[j].charAt(i) == '-') oper = -1;
		    else {
		        int l = i, left = i, right = i;
		        while (l < args[j].length() && !Character.isWhitespace(args[j].charAt(l))) {
			    right++;
			    l++;
		        }
		        i = l - 1;
		        String x = args[j].substring(left, right);
		        sum += oper * Float.parseFloat(x);
		        oper = 1;
		    }
	        }
	        i++;
	    }
	}
	System.out.println(sum);
    }
}