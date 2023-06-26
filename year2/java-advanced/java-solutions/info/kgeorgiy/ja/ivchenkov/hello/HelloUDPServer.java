package info.kgeorgiy.ja.ivchenkov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.*;

import static info.kgeorgiy.ja.ivchenkov.hello.Utils.*;

/**
 * The UDP server class.
 *
 * @author Ivchenkov Dmitrii
 */
public class HelloUDPServer extends AbstractServer {
    private ExecutorService service;
    private DatagramSocket socket;

    /**
     * Starts the server bound to the {@code port} with the {@code threads} number of working threads.
     *
     * @param port    the server port.
     * @param threads the number of working threads.
     */
    @Override
    public void start(int port, int threads) {
        try {
            socket = new DatagramSocket(port);
            service = Executors.newFixedThreadPool(threads);
            for (int i = 0; i < threads; i++) {
                service.submit(() -> {
                    try {
                        int dataSize = socket.getReceiveBufferSize();
                        DatagramPacket packet = new DatagramPacket(new byte[dataSize], dataSize);
                        while (!Thread.currentThread().isInterrupted() && !socket.isClosed()) {
                            try {
                                socket.receive(packet);
                                sendData(socket,
                                        createResponse(getPacketText(packet)).getBytes(CHARSET),
                                        packet.getSocketAddress());
                            } catch (IOException e) {
                                System.err.println("An error occurred during processing a request: " + e.getMessage());
                            }
                        }
                    } catch (SocketException e) {
                        System.err.println("Socket error occurred: " + e.getMessage());
                    }
                });
            }
        } catch (SocketException e) {
            System.err.println("Socket could not be opened or bound to the given port: " + e.getMessage());
        }
    }

    /**
     * Closes the server.
     */
    @Override
    public void close() {
        socket.close();
        service.shutdown();
        try {
            service.awaitTermination(TERMINATION_TIMEOUT, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            System.err.println("Thread was interrupted while waiting termination: " + e.getMessage());
        }
    }

    /**
     * The main method of the class to start the server.
     * <p>
     * The arguments are the server port and the threads number.
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        serverMain(args, HelloUDPServer::new);
    }
}