package info.kgeorgiy.ja.ivchenkov.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;

public class Walk {
    private final static String DIGEST_ALGORITHM = "SHA-256";

    public static void main(String[] args) {
        if (args == null || args.length != 2) {
            System.err.println("Wrong number of arguments. Please write input filename and output filename!");
            return;
        } else if (args[0] == null || args[1] == null) {
            System.err.println("Invalid arguments. Please write non-null filenames strings!");
            return;
        }

        final FileDigest fileDigest;
        try {
            fileDigest = new FileDigest(DIGEST_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            System.err.println("Unknown hashing algorithm given");
            return;
        }

        try {
            final Path inputFilePath = Path.of(args[0]);
            final Path outputFilePath = Path.of(args[1]);

            if (outputFilePath.getParent() != null) {
                Files.createDirectories(Path.of(args[1]).getParent());
            }

            try (BufferedReader in = Files.newBufferedReader(inputFilePath, StandardCharsets.UTF_8);
                 BufferedWriter out = Files.newBufferedWriter(outputFilePath, StandardCharsets.UTF_8)) {
                String fileName;
                while ((fileName = in.readLine()) != null) {
                    String fileHash = fileDigest.getFileHash(fileName);
                    out.write(fileHash + " " + fileName + System.lineSeparator());
                }
            }
        } catch (InvalidPathException e) {
            System.err.println("Invalid path given: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("An error occurred while working with files: " + e.getMessage());
        }
    }
}
