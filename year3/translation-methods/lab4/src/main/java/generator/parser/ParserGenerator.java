package generator.parser;

import generator.Rule;
import generator.symbol.NonTerminal;
import generator.symbol.Symbol;
import generator.symbol.Terminal;
import generator.symbol.TranslationSymbol;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ParserGenerator {
    private static final String LINE_SEPARATOR = System.lineSeparator();
    private static final String LINE_SEPARATOR_OFFSET = System.lineSeparator() + "    ";
    public final Set<Terminal> terminal;
    public final Set<String> nonTerminal;
    public final List<Rule> rules;
    public final Map<String, List<String>> argAttrs;
    public final Map<String, List<String>> returnAttrs;
    public Map<String, List<Rule>> rulesBy = new HashMap<>();
    public Map<String, Set<String>> first = new HashMap<>();
    public Map<String, Set<String>> follow = new HashMap<>();
    public Map<String, String> nameByValue = new HashMap<>();

    public ParserGenerator(Set<Terminal> terminal,
                           Set<String> nonTerminal,
                           List<Rule> rules,
                           Map<String, List<String>> argAttrs,
                           Map<String, List<String>> returnAttrs) {
        this.terminal = terminal;
        this.nonTerminal = nonTerminal;
        this.rules = rules;
        this.argAttrs = argAttrs;
        this.returnAttrs = returnAttrs;
        buildRulesByNonTerminal();
        buildFirstSet();
        buildFollowSet();
        for (Terminal term : terminal) {
            nameByValue.put(term.value, term.name);
        }
    }

    public boolean checkLL1() {
        for (String nonTerm : nonTerminal) {
            List<Rule> rules = rulesBy.get(nonTerm);
            for (int i = 0; i < rules.size(); i++) {
                for (int j = i + 1; j < rules.size(); j++) {
                    Set<String> first1 = getFirstBy(rules.get(i).inference);
                    Set<String> first2 = getFirst(rules.get(j).inference);
                    Set<String> intersection = new HashSet<>(first1);
                    intersection.retainAll(first2);
                    if (!intersection.isEmpty()) {
                        return false;
                    }
                    if (first1.contains("")) {
                        Set<String> follow1 = follow.get(nonTerm);
                        Set<String> intersection2 = new HashSet<>(follow1);
                        intersection2.retainAll(first2);
                        if (!intersection2.isEmpty()) {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    private Set<String> getFirstBy(List<Symbol> symbols) {
        return getFirst(
                symbols.stream()
                        .filter(symbol -> !(symbol instanceof TranslationSymbol))
                        .toList()
        );
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
                        package %s.parser;
                                                
                        import %s.lexer.Lexer;
                        import java.text.ParseException;
                        import java.util.regex.Matcher;
                        import java.util.regex.Pattern;
                                      
                        public class Parser {
                            %s
                            private Lexer lexer;
                            
                            public S parse(String string) throws ParseException {
                                lexer = new Lexer(string);
                                lexer.takeNext();
                                return parseS();
                            }
                            
                            %s
                            
                            %s
                        }
                        """,
                packageName,
                packageName,
                addOffset(
                        String.join(
                                System.lineSeparator(),
                                terminal.stream().map(term -> getMatcher(term.name, term.value)).toList()
                        )
                ),
                addOffset(
                        String.join(
                                System.lineSeparator(),
                                nonTerminal.stream().map(this::generateParseFunction).toList()
                        )
                ),
                addOffset(
                        String.join(
                                System.lineSeparator(),
                                nonTerminal.stream().map(this::generateNonTerminalClass).toList()
                        )
                )
        );
    }

    public String generateNonTerminalClass(String nonTerm) {
        return String.format(
                "public static class %s {%n%s%n}",
                nonTerm,
                addOffset(
                        String.join(
                                System.lineSeparator(),
                                returnAttrs.getOrDefault(nonTerm, List.of()).stream().map(this::generateAttributeField).toList()
                        )
                )
        );
    }

    public String generateAttributeField(String attribute) {
        String[] attr = attribute.split("::");
        return String.format("public %s %s;", attr[1].strip(), attr[0].strip());
    }

    public String generateParseFunction(String nonTerm) {
        return String.format(
                """
                        public %s parse%s(%s) throws ParseException {
                            %s %s_0 = new %s();
                            %s
                            throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
                        }
                        """,
                nonTerm,
                nonTerm,
                String.join(
                        ", ",
                        argAttrs.getOrDefault(nonTerm, List.of()).stream().map(this::generateArgument).toList()
                ),
                nonTerm,
                nonTerm,
                nonTerm,
                addOffset(
                        String.join(
                                System.lineSeparator(),
                                rulesBy.get(nonTerm).stream()
                                        .map(this::generateRuleBranch)
                                        .toList()
                        )
                )
        );
    }

    private String generateArgument(String attribute) {
        String[] attr = attribute.split("::");
        return String.format("%s %s", attr[1].strip(), attr[0].strip());
    }

    private String generateRuleBranch(Rule rule) {
        Map<String, Integer> index = new HashMap<>();
        index.put(rule.from.symbol, 1);
        String condition = String.join(
                " || ",
                getFirst1(rule).stream()
                        .map(term -> nameByValue.get(term) + "Pattern.matcher(lexer.getCurrentToken()).matches()")
                        .toList()
        );
        StringBuilder actions = new StringBuilder();
        int i = 0;
        for (Symbol symbol : rule.inference) {
            if (symbol instanceof Terminal term) {
                if (!term.value.isEmpty()) {
                    String mname = "m" + i;
                    actions.append(
                            addOffset(
                                    String.format(
                                            """
                                                    Matcher %s = %sPattern.matcher(lexer.getCurrentToken());
                                                    %s.find();
                                                    String %s = %s.group();
                                                    lexer.takeNext();
                                                    """,
                                            mname,
                                            nameByValue.get(term.value),
                                            mname,
                                            term.name,
                                            mname)
                            )
                    );
                    i++;
                }
            } else if (symbol instanceof NonTerminal nonTerm) {
                int j = index.getOrDefault(nonTerm.symbol, 0);
                actions.append(
                        String.format(
                                "%s %s_%d = parse%s(%s);%n",
                                nonTerm.symbol,
                                nonTerm.symbol,
                                j,
                                nonTerm.symbol,
                                String.join(
                                        ", ",
                                        nonTerm.arguments
                                )
                        )
                );
                index.put(nonTerm.symbol, j + 1);
            } else if (symbol instanceof TranslationSymbol transSymbol) {
                actions.append(transSymbol.code).append(System.lineSeparator());
            }
        }
        return String.format(
                "if (%s) {%n%s\treturn %s_0;%n}",
                condition,
                addOffset(actions.toString()),
                rule.from.symbol
        );
    }

    private String getMatcher(String name, String term) {
        return String.format("private static final Pattern %sPattern = Pattern.compile(\"%s\");", name, term);
    }

    private Set<String> getFirst1(Rule rule) {
        Set<String> result = getFirst(
                rule.inference.stream()
                        .filter(symbol -> !(symbol instanceof TranslationSymbol))
                        .toList()
        );
        if (result.contains("")) {
            result.remove("");
            result.addAll(follow.get(rule.from.symbol));
        }
        return result;
    }

    private void buildRulesByNonTerminal() {
        for (String n : nonTerminal) {
            rulesBy.put(n, new ArrayList<>());
        }
        for (Rule rule : rules) {
            rulesBy.get(rule.from.symbol).add(rule);
        }
    }

    private void buildFirstSet() {
        for (String nonTerm : nonTerminal) {
            first.put(nonTerm, new HashSet<>());
        }
        boolean changed = true;
        while (changed) {
            changed = false;
            for (Rule rule : rules) {
                Set<String> cur = first.get(rule.from.symbol);
                int prevSize = cur.size();
                cur.addAll(
                        getFirst(
                                rule.inference.stream()
                                        .filter(symbol -> !(symbol instanceof TranslationSymbol))
                                        .toList()
                        )
                );
                if (prevSize != cur.size()) {
                    changed = true;
                }
            }
        }
    }

    private void buildFollowSet() {
        for (String nonTerm : nonTerminal) {
            follow.put(nonTerm, new HashSet<>());
        }
        follow.get("S").add("\\\\$");
        boolean changed = true;
        while (changed) {
            changed = false;
            for (Rule rule : rules) {
                List<Symbol> inference = rule.inference.stream()
                        .filter(symbol -> !(symbol instanceof TranslationSymbol))
                        .toList();
                for (int i = 0; i < inference.size(); i++) {
                    if (inference.get(i) instanceof NonTerminal b) {
                        Set<String> followB = follow.get(b.symbol);
                        int prevSize = followB.size();
                        Set<String> firstGamma;
                        if (i + 1 < inference.size()) {
                            firstGamma = getFirst(inference.subList(i + 1, inference.size()));
                        } else {
                            firstGamma = new HashSet<>(List.of(""));
                        }
                        if (firstGamma.contains("")) {
                            firstGamma.remove("");
                            followB.addAll(follow.get(rule.from.symbol));
                        }
                        followB.addAll(firstGamma);
                        if (prevSize != followB.size()) {
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    private Set<String> getFirst(List<Symbol> symbols) {
        Set<String> result = new HashSet<>();
        if (symbols.get(0) instanceof Terminal term) {
            result.add(term.value);
        } else if (symbols.get(0) instanceof NonTerminal A) {
            result.addAll(first.get(A.symbol));
            if (result.contains("") && symbols.size() > 1) {
                result.remove("");
                result.addAll(getFirst(symbols.subList(1, symbols.size())));
            }
        }
        return result;
    }

    private static String addOffset(String code) {
        return code.replace(LINE_SEPARATOR, LINE_SEPARATOR_OFFSET);
    }
}
