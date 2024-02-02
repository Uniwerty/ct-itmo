package generator.symbol;

import jdk.jfr.Enabled;

public class Terminal implements Symbol {
    public String name;
    public String value;

    public Terminal(String name, String value) {
        this.name = name;
        this.value = value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Terminal t) {
            return name.equals(t.name) && value.equals(t.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode() + value.hashCode();
    }

    @Override
    public String toString() {
        return value;
    }
}
