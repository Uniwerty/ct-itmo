package info.kgeorgiy.ja.ivchenkov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

/**
 * The remote bank class
 *
 * @author Ivchenkov Dmitrii
 */
public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String id) throws RemoteException {
        return createInstance(id,
                RemoteAccount::new,
                accounts,
                "account");
    }

    @Override
    public Account createPersonalAccount(final Person person, final String subId) throws RemoteException {
        Account account = createAccount(person.getAccountId(subId));
        person.addAccount(subId, account);
        return account;
    }

    @Override
    public Account getAccount(final String id) {
        System.out.printf("Retrieving account %s%n", id);
        return accounts.get(id);
    }

    public Person createPerson(final String name, final String surname, final String passport) throws RemoteException {
        return createInstance(passport,
                passportId -> new RemotePerson(name, surname, passportId, new ConcurrentHashMap<>()),
                persons,
                "person");
    }

    @Override
    public Person getRemotePerson(final String passport) {
        System.out.printf("Retrieving remote person %s%n", passport);
        return persons.get(passport);
    }

    @Override
    public Person getLocalPerson(final String passport) throws RemoteException {
        System.out.printf("Retrieving local person %s%n", passport);
        if (persons.containsKey(passport)) {
            Person person = persons.get(passport);
            return new LocalPerson(person.getName(), person.getSurname(), person.getPassport(), getLocalAccounts(person));
        } else {
            return null;
        }
    }

    private <T extends Remote> T createInstance(final String id,
                                                final Function<String, T> constructor,
                                                final ConcurrentMap<String, T> map,
                                                final String className) throws RemoteException {
        System.out.printf("Creating %s %s%n", className, id);
        final T instance = constructor.apply(id);
        final T previous = map.putIfAbsent(id, instance);
        if (previous == null) {
            UnicastRemoteObject.exportObject(instance, port);
            return instance;
        } else {
            System.out.printf("The %s already exists%n", className);
            return previous;
        }
    }

    private static ConcurrentMap<String, Account> getLocalAccounts(final Person person) throws RemoteException {
        ConcurrentMap<String, Account> localAccounts = new ConcurrentHashMap<>();
        person.getAccounts().forEach((id, account) -> {
            try {
                localAccounts.put(id, new LocalAccount(id, account.getAmount()));
            } catch (RemoteException e) {
                System.err.printf("Could not retrieve money amount of %s: %s", id, e.getMessage());
            }
        });
        return localAccounts;
    }
}
