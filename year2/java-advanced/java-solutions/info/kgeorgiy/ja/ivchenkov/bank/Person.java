package info.kgeorgiy.ja.ivchenkov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.concurrent.ConcurrentMap;

/**
 * The person interface
 *
 * @author Ivchenkov Dmitrii
 */
public interface Person extends Remote {
    /**
     * Returns the person's name.
     *
     * @return the person's name
     */
    String getName() throws RemoteException;

    /**
     * Returns the person's surname.
     *
     * @return the person's surname
     */
    String getSurname() throws RemoteException;

    /**
     * Returns the person's passport.
     *
     * @return the person's passport
     */
    String getPassport() throws RemoteException;

    /**
     * Returns the map from identifiers to person's accounts.
     *
     * @return the person's accounts
     */
    ConcurrentMap<String, Account> getAccounts() throws RemoteException;

    /**
     * Returns the person's account with the specified identifier.
     *
     * @param subId the account identifier
     * @return the account with {@code subId}
     */
    Account getAccount(String subId) throws RemoteException;

    /**
     * Returns the complete identifier of the account.
     *
     * @param subId the account identifier
     * @return the complete account identifier
     */
    String getAccountId(String subId) throws RemoteException;

    /**
     * Adds the specified account to person accounts.
     *
     * @param subId   the account identifier
     * @param account the account
     */
    void addAccount(String subId, Account account) throws RemoteException;
}
