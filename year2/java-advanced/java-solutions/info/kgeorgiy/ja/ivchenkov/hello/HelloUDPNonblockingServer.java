package info.kgeorgiy.ja.ivchenkov.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.*;
import java.util.concurrent.*;

import static info.kgeorgiy.ja.ivchenkov.hello.Utils.*;

public class HelloUDPNonblockingServer extends AbstractServer {
    private Selector selector;
    private ExecutorService service;
    private ExecutorService handler;
    private SelectionKey channelKey;
    private int bufferSize;

    /**
     * Starts the server bound to the {@code port} with the {@code threads} number of working threads.
     *
     * @param port    the server port.
     * @param threads the number of working threads.
     */
    @Override
    public void start(int port, int threads) {
        try {
            selector = Selector.open();
            service = Executors.newFixedThreadPool(threads);
            handler = Executors.newSingleThreadExecutor();
            DatagramChannel channel = DatagramChannel.open();
            bufferSize = channel.socket().getReceiveBufferSize();
            channelKey = channel.bind(new InetSocketAddress(port))
                    .configureBlocking(false)
                    .register(selector,
                            SelectionKey.OP_READ,
                            new ServerContext()
                    );
            handler.submit(this::launch);
        } catch (IOException e) {
            System.err.println("Some IO error occurred while starting: " + e.getMessage());
        }
    }

    /**
     * Closes the server.
     */
    @Override
    public void close() {
        try {
            channelKey.channel().close();
            selector.close();
            service.shutdown();
            service.awaitTermination(TERMINATION_TIMEOUT, TimeUnit.SECONDS);
            handler.shutdown();
            handler.awaitTermination(TERMINATION_TIMEOUT, TimeUnit.SECONDS);
        } catch (IOException e) {
            System.err.println("Some IO error occurred while closing the server: " + e.getMessage());
        } catch (InterruptedException e) {
            System.err.println("Thread was interrupted while waiting termination: " + e.getMessage());
        }
    }

    /**
     * The main method of the class to start the server.
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        serverMain(args, HelloUDPNonblockingServer::new);
    }

    private void launch() {
        while (!Thread.currentThread().isInterrupted() && selector.isOpen()) {
            try {
                selector.select(key -> {
                    if (key.isReadable()) {
                        receiveRequest(key);
                    } else if (key.isWritable()) {
                        sendResponse(key);
                    }
                });
            } catch (IOException e) {
                System.err.println("Some IO error occurred with the selector: " + e.getMessage());
            }
        }
    }

    private void receiveRequest(SelectionKey key) {
        final ServerContext context = (ServerContext) key.attachment();
        final DatagramChannel channel = (DatagramChannel) key.channel();
        try {
            ByteBuffer buffer = ByteBuffer.allocate(bufferSize);
            SocketAddress destination = channel.receive(buffer);
            buffer.flip();
            service.submit(() -> {
                String response = createResponse(getBufferText(buffer));
                buffer.clear();
                buffer.put(response.getBytes(CHARSET));
                buffer.flip();
                context.addResponse(buffer, destination);
            });
        } catch (IOException e) {
            System.err.println("Some IO error occurred while receiving request: " + e.getMessage());
        }
    }

    private void sendResponse(SelectionKey key) {
        final ServerContext context = (ServerContext) key.attachment();
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final Response response = context.getResponse();
        try {
            channel.send(response.buffer, response.destination);
        } catch (IOException e) {
            System.err.println("Some IO error occurred while sending response: " + e.getMessage());
        }
    }

    private class ServerContext {
        private final Queue<Response> responses;

        private ServerContext() {
            responses = new ArrayDeque<>();
        }

        private synchronized Response getResponse() {
            Response response = responses.remove();
            if (responses.isEmpty()) {
                channelKey.interestOpsAnd(~SelectionKey.OP_WRITE);
                selector.wakeup();
            }
            return response;
        }

        private synchronized void addResponse(ByteBuffer buffer, SocketAddress address) {
            if (responses.isEmpty()) {
                channelKey.interestOpsOr(SelectionKey.OP_WRITE);
                selector.wakeup();
            }
            responses.add(new Response(buffer, address));
        }
    }

    private record Response(ByteBuffer buffer, SocketAddress destination) {
    }
}
