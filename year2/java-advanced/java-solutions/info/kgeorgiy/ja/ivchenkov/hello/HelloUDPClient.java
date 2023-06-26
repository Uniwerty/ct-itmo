package info.kgeorgiy.ja.ivchenkov.hello;

import java.io.IOException;
import java.net.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static info.kgeorgiy.ja.ivchenkov.hello.Utils.*;

/**
 * The UDP client class.
 *
 * @author Ivchenkov Dmitrii
 */
public class HelloUDPClient extends AbstractClient {
    private static final long TERMINATION_TIMEOUT = 5000;

    /**
     * Runs the client, sends requests, waits for server responses and prints them.
     *
     * @param host     the server host
     * @param port     the server port
     * @param prefix   the request prefix
     * @param threads  the number of request threads
     * @param requests the number of requests per thread.
     */
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try {
            final SocketAddress address = new InetSocketAddress(InetAddress.getByName(host), port);
            final ExecutorService service = Executors.newFixedThreadPool(threads);

            for (int thread = 1; thread <= threads; thread++) {
                final int threadNumber = thread;
                service.submit(() -> {
                    processRequest(prefix, requests, address, threadNumber);
                });
            }
            service.shutdown();
            service.awaitTermination(TERMINATION_TIMEOUT * threads * requests, TimeUnit.SECONDS);
        } catch (UnknownHostException e) {
            System.err.println("Unknown host given: " + e.getMessage());
        } catch (InterruptedException e) {
            System.err.println("Thread was interrupted while waiting termination: " + e.getMessage());
        }
    }

    /**
     * The main method of the class to run the client.
     * <p>
     * The arguments are the name or the IP address of the server host, the server port,
     * the request prefix, the number of client threads and the number of requests in each thread.
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        clientMain(args, HelloUDPClient::new);
    }

    private void processRequest(String prefix, int requests, SocketAddress address, int threadNumber) {
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(RECEIVING_TIMEOUT);
            for (int requestNumber = 1; requestNumber <= requests; requestNumber++) {
                final String request = createRequest(prefix, threadNumber, requestNumber);
                final byte[] data = request.getBytes(CHARSET);
                int dataSize = socket.getReceiveBufferSize();
                DatagramPacket packet = new DatagramPacket(new byte[dataSize], dataSize);
                while (!Thread.currentThread().isInterrupted() && !socket.isClosed()) {
                    try {
                        sendData(socket, data, address);
                        socket.receive(packet);
                        final String response = getPacketText(packet);
                        if (checkResponse(response, request)) {
                            System.out.printf("Request sent: %s%n", request);
                            System.out.printf("Response received: %s%n", response);
                            break;
                        }
                    } catch (IOException e) {
                        System.err.println("An error occurred during processing a request: " + e.getMessage());
                    }
                }
            }
        } catch (SocketException e) {
            System.err.println("Socket could not be opened: " + e.getMessage());
        }
    }
}