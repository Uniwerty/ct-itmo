package generator.symbol;

import java.util.ArrayList;
import java.util.List;

public class NonTerminal implements Symbol {
    public String symbol;
    public List<String> arguments = new ArrayList<>();

    public NonTerminal(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }
}
