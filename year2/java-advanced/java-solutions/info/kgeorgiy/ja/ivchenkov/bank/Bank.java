package info.kgeorgiy.ja.ivchenkov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * The bank interface.
 *
 * @author Ivchenkov Dmitrii
 */
public interface Bank extends Remote {
    /**
     * Creates a new account with the specified identifier if it does not already exist.
     *
     * @param id the account id
     * @return the created or existing {@link Account}
     */
    Account createAccount(String id) throws RemoteException;

    /**
     * Creates a new account of the specified person with the specified identifier.
     *
     * @param person {@link Person} whose account is to be created
     * @param subId  the personal account identifier
     * @return the created or existing {@link Account}
     * @throws RemoteException
     */
    Account createPersonalAccount(Person person, String subId) throws RemoteException;

    /**
     * Returns an account by identifier.
     *
     * @param id the account identifier
     * @return the account with the specified identifier or {@code null} if such account does not exist.
     */
    Account getAccount(String id) throws RemoteException;

    /**
     * Creates a person with the specified data.
     *
     * @param name     the person's name
     * @param surname  the person's surname
     * @param passport the person's passport
     * @return the {@link Person} with the specified data.
     */
    Person createPerson(String name, String surname, String passport) throws RemoteException;

    /**
     * Returns the remote person with the specified passport.
     *
     * @param passport the person's passport
     * @return the {@link RemotePerson} with the specified passport,
     * or {@code null} if such person does not exist.
     */
    Person getRemotePerson(String passport) throws RemoteException;

    /**
     * Returns the local person with the specified passport.
     *
     * @param passport the person's passport
     * @return the {@link LocalPerson} with the specified passport,
     * or {@code null} if such person does not exist.
     */
    Person getLocalPerson(String passport) throws RemoteException;
}
