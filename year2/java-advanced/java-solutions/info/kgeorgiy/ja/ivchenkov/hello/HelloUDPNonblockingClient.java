package info.kgeorgiy.ja.ivchenkov.hello;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;

import static info.kgeorgiy.ja.ivchenkov.hello.Utils.*;

public class HelloUDPNonblockingClient extends AbstractClient {
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try {
            final SocketAddress address = new InetSocketAddress(InetAddress.getByName(host), port);
            final Selector selector = Selector.open();
            for (int thread = 1; thread <= threads; thread++) {
                DatagramChannel channel = DatagramChannel.open();
                channel.connect(address)
                        .configureBlocking(false)
                        .register(selector,
                                SelectionKey.OP_WRITE,
                                new KeyContext(
                                        ByteBuffer.allocate(channel.socket().getReceiveBufferSize()),
                                        prefix,
                                        thread,
                                        requests)
                        );
            }
            while (!Thread.currentThread().isInterrupted() && !selector.keys().isEmpty()) {
                int consumed = selector.select(key -> {
                    if (key.isReadable()) {
                        receiveResponse(key);
                    } else if (key.isWritable()) {
                        sendRequest(key);
                    }
                }, RECEIVING_TIMEOUT);
                if (consumed == 0) {
                    selector.keys().stream().filter(SelectionKey::isWritable).forEach(this::sendRequest);
                }
            }
            selector.close();
        } catch (UnknownHostException e) {
            System.err.println("Unknown host given: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Some IO error occurred: " + e.getMessage());
        }
    }

    /**
     * The main method of the class to run the client.
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        clientMain(args, HelloUDPNonblockingClient::new);
    }

    private void receiveResponse(SelectionKey key) {
        final KeyContext context = (KeyContext) key.attachment();
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final ByteBuffer buffer = context.getBuffer().clear();
        try {
            channel.receive(buffer);
            buffer.flip();
            final String request = context.getRequest();
            final String response = getBufferText(buffer);
            if (response.contains(request)) {
                System.out.printf("Request sent: %s%n", request);
                System.out.printf("Response received: %s%n", response);
                context.incrementRequestNumber();
            }
        } catch (IOException e) {
            System.err.println("Some IO error occurred during receiving response: " + e.getMessage());
        }
        if (context.requestsRemain()) {
            key.interestOps(SelectionKey.OP_WRITE);
        } else {
            try {
                key.channel().close();
            } catch (IOException e) {
                System.err.println("Some IO error occurred during closing channel: " + e.getMessage());
            }
        }
    }

    private void sendRequest(SelectionKey key) {
        final KeyContext context = (KeyContext) key.attachment();
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final ByteBuffer buffer = context.getBuffer().clear();
        buffer.put(context.getRequest().getBytes(CHARSET)).flip();
        try {
            channel.send(buffer, channel.getRemoteAddress());
            buffer.flip();
            key.interestOps(SelectionKey.OP_READ);
        } catch (IOException e) {
            System.err.println("Some IO error occurred during sending request: " + e.getMessage());
        }
    }

    private static class KeyContext {
        private final ByteBuffer buffer;
        private final String requestPrefix;
        private final int thread;
        private final int requests;
        private int requestNumber;

        private KeyContext(ByteBuffer buffer, String prefix, int thread, int requests) {
            this.buffer = buffer;
            this.requestPrefix = prefix;
            this.thread = thread;
            this.requests = requests;
            requestNumber = 1;
        }

        public ByteBuffer getBuffer() {
            return buffer;
        }

        public String getRequest() {
            return createRequest(requestPrefix, thread, requestNumber);
        }

        public void incrementRequestNumber() {
            requestNumber++;
        }

        public boolean requestsRemain() {
            return requestNumber <= requests;
        }
    }
}