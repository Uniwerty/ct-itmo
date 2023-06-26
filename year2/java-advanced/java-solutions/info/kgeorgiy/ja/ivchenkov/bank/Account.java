package info.kgeorgiy.ja.ivchenkov.bank;

import java.rmi.*;

/**
 * The bank account interface.
 *
 * @author Ivchenkov Dmitrii
 */
public interface Account extends Remote {
    /**
     * Returns account identifier.
     */
    String getId() throws RemoteException;

    /**
     * Returns amount of money in the account.
     */
    long getAmount() throws RemoteException;

    /**
     * Sets amount of money in the account.
     *
     * @param amount the amount of money to set
     */
    void setAmount(long amount) throws RemoteException;

    /**
     * Adds amount of money in the account.
     *
     * @param amount the amount of money to add
     */
    void addAmount(long amount) throws RemoteException;
}
