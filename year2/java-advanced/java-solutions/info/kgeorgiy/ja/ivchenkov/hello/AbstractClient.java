package info.kgeorgiy.ja.ivchenkov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Supplier;

public abstract class AbstractClient implements HelloClient {
    protected static final int RECEIVING_TIMEOUT = 300;

    @Override
    public abstract void run(String host, int port, String prefix, int threads, int requests);

    protected static String createRequest(String prefix, int thread, int request) {
        return String.format("%s%d_%d", prefix, thread, request);
    }

    protected static boolean checkResponse(String response, String request) {
        return response.equals(String.format("%s %s", Utils.SERVER_RESPONSE_PREFIX, request));
    }

    protected static void clientMain(String[] args, Supplier<HelloClient> clientImplementation) {
        if (args == null || args.length != 5) {
            System.err.println("Invalid number of arguments! Please write: host port prefix threads requests");
            return;
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Invalid arguments given: arguments must be non-null values!");
            return;
        }
        try {
            HelloClient client = clientImplementation.get();
            client.run(
                    args[0],
                    Integer.parseInt(args[1]),
                    args[2],
                    Integer.parseInt(args[3]),
                    Integer.parseInt(args[4]));
        } catch (NumberFormatException e) {
            System.err.println("Invalid arguments given: port, threads, requests must be positive integers!");
        }
    }
}
