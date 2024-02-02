package generator.symbol;

public class TranslationSymbol implements Symbol {
    public String code;

    public TranslationSymbol(String code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return code;
    }
}
