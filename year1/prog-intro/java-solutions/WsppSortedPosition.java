import java.util.*;
import java.io.*;

class MyPair {
    int a, b;
    public MyPair(int a, int b) {
	this.a = a;
	this.b = b;
    }
}

public class WsppSortedPosition {
    public static void main(final String[] args) {
	try {
	    Map<String, List<MyPair>> dictionary = new TreeMap<>();
	    int i = 1;
	    Scanner in = new Scanner(new File(args[0]), "utf-8");
	    while (in.hasNextLine()) {
		int j = 1;
		String line = in.nextLine();
		int left = 0, right = 0;
		while (right < line.length()) {
		    while (right < line.length() && (Character.isLetter(line.charAt(right)) || line.charAt(right) == '\'' ||
		    Character.getType(line.charAt(right)) == Character.DASH_PUNCTUATION)) {
			right++;
		    }
		    String newWord = line.substring(left, right).toLowerCase();
		    if (newWord.length() != 0) {
		        List<MyPair> count;
		        if (dictionary.containsKey(newWord)) {
			    count = dictionary.get(newWord);
			    count.set(0, new MyPair(count.get(0).a + 1, count.get(0).b + 1));
		        }
	                else {
			    count = new ArrayList<>();
			    count.add(new MyPair(1, 1));
		        }
		        count.add(new MyPair(i, j));
		        dictionary.put(newWord, count);
		        j++;
		    }
		    right++;
		    left = right;
		}
		i++;
	    }
	    in.close();

	    BufferedWriter out = new BufferedWriter(
		new OutputStreamWriter(
		    new FileOutputStream(args[1]),
		"utf-8"
		)
	    );
	    StringBuilder sb = new StringBuilder();
	    for (Map.Entry<String, List<MyPair>> entry : dictionary.entrySet()) {
		sb.append(entry.getKey()).append(" ");
		sb.append(entry.getValue().get(0).a).append(" ");
		for (int j = 1; j < entry.getValue().size(); j++) {
		    sb.append(entry.getValue().get(j).a).append(":").append(entry.getValue().get(j).b);
		    if (j < entry.getValue().size() - 1) sb.append(" ");
		}
		sb.append(System.lineSeparator());
	    }
	    out.write(sb.toString());
	    out.close();
	}
	catch (IOException e) {
	    System.out.println("Scanner error: " + e.getMessage());
	}
    }
}