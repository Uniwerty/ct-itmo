package info.kgeorgiy.ja.ivchenkov.bank;

import java.rmi.*;
import java.rmi.server.*;
import java.net.*;

/**
 * The server application class
 *
 * @author Ivchenkov Dmitrii
 */
public final class Server {
    private final static int DEFAULT_PORT = 8888;

    public static void main(final String[] args) {
        try {
            final int port = args != null && args.length > 0 ? Integer.parseInt(args[0]) : DEFAULT_PORT;
            final Bank bank = new RemoteBank(port);
            UnicastRemoteObject.exportObject(bank, port);
            Naming.rebind("//localhost/bank", bank);
            System.out.println("Server started");
        } catch (final RemoteException e) {
            System.err.println("Cannot export object: " + e.getMessage());
        } catch (final MalformedURLException e) {
            System.err.println("Malformed URL given: " + e.getMessage());
        }
    }
}
