package generator.lexer;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;

public class LexerGenerator {
    private final List<String> terminal;
    private final String skip;

    public LexerGenerator(List<String> terminal, String skip) {
        this.terminal = terminal;
        this.terminal.sort(Comparator.comparingInt(String::length).reversed());
        this.skip = skip;
    }

    public void generateFile(Path filepath, String packageName) throws IOException {
        if (Files.notExists(filepath)) {
            Files.createFile(filepath);
        }
        Files.writeString(filepath, generateClass(packageName), StandardCharsets.UTF_8);
    }

    private String generateClass(String packageName) {
        return String.format(
                """
                        package %s.lexer;
                        
                        import java.util.ArrayList;
                        import java.util.List;
                        import java.util.regex.Matcher;
                        import java.util.regex.Pattern;
                        
                        public class Lexer {
                            private final String string;
                            private final List<Matcher> tokenMatchers;
                            private final Matcher skipMatcher;
                            private int position;
                            private String currentToken;
                            
                            public Lexer(String string) {
                                this.string = string;
                                tokenMatchers = new ArrayList<>();
                                %s
                            }
                            
                            public int getPosition() {
                                return position;
                            }
                            
                            public String getCurrentToken() {
                                return currentToken;
                            }
                            
                            public void takeNext() {
                                skip();
                                if (!hasNext()) {
                                    currentToken = "$";
                                    return;
                                }
                                for (Matcher matcher : tokenMatchers) {
                                    if (matcher.find(position) && matcher.start() == position) {
                                        currentToken = matcher.group();
                                        position = matcher.end();
                                        return;
                                    }
                                }
                            }
                            
                            private void skip() {
                                if (skipMatcher.find(position) && skipMatcher.start() == position) {
                                    position = skipMatcher.end();
                                }
                            }
                            
                            private boolean hasNext() {
                                return position < string.length();
                            }
                        }
                        """,
                packageName,
                generateMatchers()
        );
    }

    private String generateMatchers() {
        StringBuilder stringBuilder = new StringBuilder();
        for (String token : terminal) {
            stringBuilder.append(
                    String.format(
                            "tokenMatchers.add(Pattern.compile(\"%s\").matcher(string));%n",
                            token
                    )
            );
        }
        stringBuilder.append(
                String.format(
                        "skipMatcher = Pattern.compile(%s).matcher(string);%n",
                        skip
                )
        );
        return stringBuilder.toString();
    }


}
