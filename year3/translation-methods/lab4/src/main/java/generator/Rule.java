package generator;

import generator.symbol.NonTerminal;
import generator.symbol.Symbol;

import java.util.ArrayList;
import java.util.List;

public class Rule {
    public NonTerminal from;
    public List<Symbol> inference;

    public Rule(NonTerminal from) {
        this.from = from;
        this.inference = new ArrayList<>();
    }

    @Override
    public String toString() {
        return String.format("%s -> %s", from.toString(), inference.toString());
    }
}
