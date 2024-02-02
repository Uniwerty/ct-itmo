package converter;

import java.nio.file.Path;

public class Application {
    public static void main(String[] args) {
        try {
            FileConverter.convert(Path.of(args[0]), Path.of(args[1]));
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }
}
