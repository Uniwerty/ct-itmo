import java.io.*;

public class WordStatInput {
    public static void main(final String[] args) {
        try {
            CustomPair[] words = new CustomPair[10];
            int i = 0;

            BufferedReader in = new BufferedReader(
                    new InputStreamReader(
                            new FileInputStream(args[0]),
                            "utf-8"
                    )
            );
            try {
                while (true) {
                    String line = in.readLine();
                    if (line == null) {
                        break;
                    }
                    int left = 0, right = 0;
                    while (right < line.length()) {
                        while (right < line.length() && (Character.isLetter(line.charAt(right)) || line.charAt(right) == '\'' ||
                                Character.getType(line.charAt(right)) == Character.DASH_PUNCTUATION)) {
                            right++;
                        }
                        String newWord = line.substring(left, right).toLowerCase();
                        if (newWord.length() != 0) {
                            int index = -1;
                            for (int j = 0; j < i; j++) {
                                if (newWord.equals(words[j].first)) {
                                    index = j;
                                }
                            }
                            if (index != -1) {
                                CustomPair p = new CustomPair(words[index].first, words[index].second + 1);
                                words[index] = p;
                            } else {
                                if (words.length <= i) {
                                    CustomPair[] newWords = new CustomPair[words.length * 2];
                                    for (int j = 0; j < words.length; j++) {
                                        newWords[j] = words[j];
                                    }
                                    words = newWords;
                                }
                                CustomPair p = new CustomPair(newWord, 1);
                                words[i] = p;
                                i++;
                            }
                        }
                        left = right + 1;
                        right++;
                    }
                }
            } finally {
                in.close();
            }

            BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                            new FileOutputStream(args[1]),
                            "utf-8"
                    )
            );
            try {
                for (int j = 0; j < i; j++) {
                    out.write(words[j].first + " " + words[j].second + "\n");
                }
            } finally {
                out.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("Cannot open file: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Cannot read or write file: " + e.getMessage());
        }
    }
}