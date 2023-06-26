package info.kgeorgiy.ja.ivchenkov.walk;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class FileDigest {
    private static final int BUFFER_SIZE = 1 << 12;
    private static final String BLANK_HASH = "0".repeat(64);
    private final MessageDigest digest;
    private final byte[] buffer;

    public FileDigest(String digestAlgorithm) throws NoSuchAlgorithmException {
        this.digest = MessageDigest.getInstance(digestAlgorithm);
        this.buffer = new byte[BUFFER_SIZE];
    }

    public String getFileHash(String file) {
        String hashString;
        try (InputStream fileReader = Files.newInputStream(Path.of(file))) {
            int bytesCount;
            while ((bytesCount = fileReader.read(buffer)) >= 0) {
                digest.update(buffer, 0, bytesCount);
            }
            hashString = getHexString(digest.digest());
            digest.reset();
            return hashString;
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.getMessage());
        } catch (InvalidPathException e) {
            System.err.println("Invalid path given: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("An error occurred while working with files: " + e.getMessage());
        }
        return BLANK_HASH;
    }

    private String getHexString(byte[] byteArray) {
        StringBuilder stringBuilder = new StringBuilder();
        for (byte b : byteArray) {
            stringBuilder.append(String.format("%02x", Byte.toUnsignedInt(b)));
        }
        return stringBuilder.toString();
    }
}
