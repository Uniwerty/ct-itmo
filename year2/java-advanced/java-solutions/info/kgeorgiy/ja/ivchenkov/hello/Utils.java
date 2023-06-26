package info.kgeorgiy.ja.ivchenkov.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

class Utils {
    static final Charset CHARSET = StandardCharsets.UTF_8;
    static final String SERVER_RESPONSE_PREFIX = "Hello,";

    static void sendData(DatagramSocket socket, byte[] data, SocketAddress address) throws IOException {
        socket.send(new DatagramPacket(data, data.length, address));
    }

    static String getPacketText(DatagramPacket packet) {
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), CHARSET);
    }

    static String getBufferText(ByteBuffer buffer) {
        return new String(buffer.array(), 0, buffer.limit(), CHARSET);
    }
}
