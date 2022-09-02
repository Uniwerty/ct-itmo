package md2html;

import md2html.markup.*;

import java.io.*;
import java.util.*;

public class Md2Html {
    private static List<String> strings;

    public static void main(String[] args) {
        try {
            strings = new ArrayList<>();
            convert(args[0], args[1]);
        } catch (IOException e) {
            System.out.println("Converter error: " + e.getMessage());
        }
    }

    public static void readMarkdown(String input) throws IOException {
        Scanner scanner = new Scanner(new File(input), "utf8");
        StringBuilder str = new StringBuilder();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.isEmpty() && !str.isEmpty()) {
                strings.add(str.toString().substring(0,
                        str.toString().length() - System.lineSeparator().length()));
                str.setLength(0);
            } else if (!line.isEmpty()) {
                str.append(line);
                str.append(System.lineSeparator());
            }
        }
        if (!str.isEmpty()) strings.add(str.toString().substring(0,
                str.toString().length() - System.lineSeparator().length()));
        scanner.close();
    }

    public static void parseMarkdown(String line, String type, int l, int r, List<Markdown> list) {
        List<Markdown> markdowns = new ArrayList<>();
        int i = l;
        while (i < r) {
            if (i < r - 1 && ((line.charAt(i) == '*' && line.charAt(i+1) == '*')
                    || (line.charAt(i) == '_' && line.charAt(i+1) == '_'))) {
                // Check strong
                char cur = line.charAt(i);
                int j = i + 2;
                while (j < r - 1 && !(line.charAt(j) == cur && line.charAt(j+1) == cur)) {
                    j++;
                }
                if (j < r - 1) {
                    markdowns.add(new Text(line.substring(l, i)));
                    parseMarkdown(line, "strong", i + 2, j, markdowns);
                    l = j + 2;
                    i = l;
                } else i++;
            } else if (i < r - 1 && line.charAt(i) == '-' && line.charAt(i + 1) == '-') {
                // Check strikeout
                int j = i + 2;
                while (j < r - 1 && !(line.charAt(j) == '-' && line.charAt(j + 1) == '-')) {
                    j++;
                }
                if (j < r - 1) {
                    markdowns.add(new Text(line.substring(l, i)));
                    parseMarkdown(line, "s", i + 2, j, markdowns);
                    l = j + 2;
                    i = l;
                } else i++;
            } else if ((i > l && line.charAt(i-1) != '\\' || i == l) && (line.charAt(i) == '*'
                    || line.charAt(i) == '_')) {
                // Check emphasis
                char cur = line.charAt(i);
                int j = i + 1;
                while (j < r && !(line.charAt(j) == cur && line.charAt(j-1) != '\\' && line.charAt(j + 1) != cur && line.charAt(j - 1) != cur)) {
                    j++;
                }
                if (j < r) {
                    markdowns.add(new Text(line.substring(l, i)));
                    parseMarkdown(line, "em", i + 1, j, markdowns);
                    l = j + 1;
                    i = l;
                } else i++;
            } else if (line.charAt(i) == '`') {
                // Check code
                int j = i + 1;
                while (j < r && line.charAt(j) != '`') {
                    j++;
                }
                if (j < r) {
                    markdowns.add(new Text(line.substring(l, i)));
                    parseMarkdown(line, "code", i + 1, j, markdowns);
                    l = j + 1;
                    i = l;
                } else i++;
            } else if (i < r - 1 && (i > l && line.charAt(i-1) != '\\' || i == 0) && line.charAt(i) == '\'' && line.charAt(i + 1) == '\'') {
                // Check quote
                int j = i + 2;
                while (j < r - 1 && !(line.charAt(j) == '\'' && line.charAt(j + 1) == '\'' && line.charAt(j-1) != '\\')) {
                    j++;
                }
                if (j < r - 1) {
                    markdowns.add(new Text(line.substring(l, i)));
                    parseMarkdown(line, "q", i + 2, j, markdowns);
                    l = j + 2;
                    i = l;
                } else i++;
            } else if (i > l && line.charAt(i-1) == '\\' && (line.charAt(i) == '*' || line.charAt(i) == '_' || line.charAt(i) == '\'')) {
                // Screen symbols
                StringBuilder newLine = new StringBuilder();
                newLine.append(line.substring(l, i - 1));
                newLine.append(line.substring(i, r));
                line = newLine.toString();
                r -= 1;
            } else i++;
        }
        String ending = line.substring(l, r);
        if (!ending.isEmpty()) markdowns.add(new Text(ending));
        switch (type){
            case "p":
                list.add(new Paragraph(markdowns));
                break;
            case "h":
                list.add(new Heading(markdowns));
                break;
            case "strong":
                list.add(new Strong(markdowns));
                break;
            case "s":
                list.add(new Strikeout(markdowns));
                break;
            case "em":
                list.add(new Emphasis(markdowns));
                break;
            case "code":
                list.add(new Code(markdowns));
                break;
            case "q":
                list.add(new Quote(markdowns));
                break;
        }
    }

    public static void convert(String input, String output) throws IOException{
        BufferedWriter writer = new BufferedWriter(
                new OutputStreamWriter(
                        new FileOutputStream(output), "utf8"
                )
        );
        readMarkdown(input);
        for (String line : strings) {
            String type = "p";
            int l = 0;
            int r = line.length();
            int level = 0;
            // Check heading
            if (line.charAt(0) == '#') {
                int i = 0;
                while (i < line.length() && line.charAt(i) == '#') {
                    i++;
                }
                if (i < line.length() && line.charAt(i) == ' ') {
                    type = "h";
                    level = i;
                    l = i + 1;
                }
            }
            int i = l;
            // Screen special HTML symbols
            while (i < r) {
                if (line.charAt(i) == '<' || line.charAt(i) == '>' || line.charAt(i) == '&') {
                    char mChar = line.charAt(i);
                    String hChar = "";
                    switch (mChar) {
                        case '<':
                            hChar = "&lt;";
                            break;
                        case '>':
                            hChar = "&gt;";
                            break;
                        case '&':
                            hChar = "&amp;";
                            break;
                    }
                    StringBuilder newLine = new StringBuilder();
                    newLine.append(line.substring(l, i));
                    newLine.append(hChar);
                    newLine.append(line.substring(i + 1, r));
                    line = newLine.toString();
                    i += hChar.length();
                    r += hChar.length() - 1;
                } else i++;
            }
            List<Markdown> markdowns = new ArrayList<>();
            parseMarkdown(line, type, l, r, markdowns);
            for (Markdown markdown : markdowns) {
                StringBuilder html = new StringBuilder();
                markdown.toHtml(html, level);
                html.append(System.lineSeparator());
                writer.write(html.toString());
            }
        }
        writer.close();
    }
}