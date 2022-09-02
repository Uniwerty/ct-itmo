import java.nio.charset.StandardCharsets;
import java.util.*;
import java.io.*;
import java.util.Scanner;

public class Wspp {
    public static void main(final String[] args) {
        try {
            Map<String, List<Integer>> dictionary = new LinkedHashMap<>();
            int i = 1;
            int j = 1;
            Scanner in = new Scanner(new File(args[0]), StandardCharsets.UTF_8);
            while (in.hasNextLine()) {
                String line = in.nextLine();
                int left = 0, right = 0;
                while (right < line.length()) {
                    while (right < line.length() && (Character.isLetter(line.charAt(right)) || line.charAt(right) == '\'' ||
                            Character.getType(line.charAt(right)) == Character.DASH_PUNCTUATION)) {
                        right++;
                    }
                    String newWord = line.substring(left, right).toLowerCase();
                    if (newWord.length() != 0) {
                        List<Integer> count;
                        if (dictionary.containsKey(newWord)) {
                            count = dictionary.get(newWord);
                            count.set(0, count.get(0) + 1);
                        }
                        else {
                            count = new ArrayList<>();
                            count.add(1);
                        }
                        count.add(j);
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
                            StandardCharsets.UTF_8
                    )
            );
            StringBuilder sb = new StringBuilder();
            for (Map.Entry<String, List<Integer>> entry : dictionary.entrySet()) {
                sb.append(entry.getKey()).append(" ");
                for (int k = 0; k < entry.getValue().size(); k++) {
                    sb.append(entry.getValue().get(k));
                    if (k < entry.getValue().size() - 1) sb.append(" ");
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