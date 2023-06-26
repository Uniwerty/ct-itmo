package info.kgeorgiy.ja.ivchenkov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Supplier;

import static info.kgeorgiy.ja.ivchenkov.hello.Utils.SERVER_RESPONSE_PREFIX;

public abstract class AbstractServer implements HelloServer {
    protected static final long TERMINATION_TIMEOUT = 100;

    @Override
    public abstract void start(int port, int threads);

    @Override
    public abstract void close();

    protected static String createResponse(String request) {
        return String.format("%s %s", SERVER_RESPONSE_PREFIX, request);
    }

    protected static void serverMain(String[] args, Supplier< HelloServer> serverImplementation) {
        if (args == null || args.length != 2) {
            System.err.println("Invalid number of arguments! Please write: port threads");
            return;
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Invalid arguments given: arguments must be non-null values!");
            return;
        }
        try (HelloServer server = serverImplementation.get();
             InputStreamReader in = new InputStreamReader(System.in)) {
            server.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
            char[] buffer = new char[5];
            while (true) {
                if (in.read(buffer) > 0 && new String(buffer).equals("close")) {
                    break;
                }
            }
        } catch (IOException e) {
            System.err.println("IOError occurred: " + e.getMessage());
        } catch (NumberFormatException e) {
            System.err.println("Invalid arguments given: arguments must be positive integers!: " + e.getMessage());
        }
    }
}
