package info.kgeorgiy.ja.ivchenkov.bank;


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

/**
 * The testing class.
 *
 * @author Ivchenkov Dmitrii
 */
public class BankTest {
    private static final String BANK_URL = "//localhost/bank";
    private static final int PORT = 8888;
    private static Bank bank;

    @Before
    public void createRegistryAndBank() {
        try {
            LocateRegistry.createRegistry(PORT);
            bank = new RemoteBank(PORT);
            UnicastRemoteObject.exportObject(bank, PORT);
            Naming.rebind(BANK_URL, bank);
        } catch (RemoteException ignored) {
        } catch (MalformedURLException e) {
            System.err.println("Invalid URL given");
        }
    }

    @After
    public void unexportBank() {
        try {
            UnicastRemoteObject.unexportObject(bank, false);
        } catch (RemoteException ignored) {
        }
    }

    @Test
    public void createAndGetLocalPersonTest() throws RemoteException {
        createAndGetTest("Dmitry", "Ivchenkov", "777", passport -> {
            try {
                return bank.getLocalPerson(passport);
            } catch (RemoteException e) {
                System.err.println("Remote error occurred: " + e.getMessage());
                return null;
            }
        });
    }

    @Test
    public void createAndGetRemotePersonTest() throws RemoteException {
        createAndGetTest("Ivan", "Ivanov", "666", passport -> {
            try {
                return bank.getRemotePerson(passport);
            } catch (RemoteException e) {
                System.err.println("Remote error occurred: " + e.getMessage());
                return null;
            }
        });
    }

    private void createAndGetTest(String name,
                                  String surname,
                                  String passport,
                                  Function<String, Person> getter) throws RemoteException {
        bank.createPerson(name, surname, passport);
        Person person = getter.apply(passport);
        Assert.assertNotNull(person);
        Assert.assertEquals(name, person.getName());
        Assert.assertEquals(surname, person.getSurname());
        Assert.assertEquals(passport, person.getPassport());
    }

    @Test
    public void getNonExistingPerson() throws RemoteException {
        Assert.assertNull(bank.getRemotePerson("nonExistingPerson"));
    }

    @Test
    public void duplicateCreatePersonTest() throws RemoteException {
        String name = "Petr";
        String surname = "Petrov";
        String passport = "555";
        Person person1 = bank.createPerson(name, surname, passport);
        Person person2 = bank.createPerson(name, surname, passport);
        Assert.assertEquals(person1, person2);
    }

    @Test
    public void createAndGetAccountTest() throws RemoteException {
        String id = "simpleAccount";
        bank.createAccount(id);
        Account account = bank.getAccount(id);
        Assert.assertNotNull(account);
        Assert.assertEquals(id, account.getId());
        Assert.assertEquals(0, account.getAmount());
    }

    @Test
    public void getNonExistingAccountTest() throws RemoteException {
        Assert.assertNull(bank.getAccount("nonExistingAccount"));
    }

    @Test
    public void duplicateCreateAccountTest() throws RemoteException {
        String id = "duplicatedAccount";
        Account account1 = bank.createAccount(id);
        Account account2 = bank.createAccount(id);
        Assert.assertEquals(account1, account2);
    }

    @Test
    public void createAndGetPersonalAccountTest() throws RemoteException {
        Person person = bank.createPerson("Smir", "Smirnov", "444");
        String subId = "0";
        bank.createPersonalAccount(person, subId);
        Account account = bank.getAccount(person.getAccountId(subId));
        Assert.assertNotNull(account);
        Assert.assertEquals(account, person.getAccount(subId));
    }

    @Test
    public void createManyPersonalAccountsTest() throws RemoteException {
        Person person = bank.createPerson("Maxim", "Maximov", "333");
        for (int i = 0; i < 5; i++) {
            bank.createPersonalAccount(person, String.valueOf(i));
        }
        Assert.assertEquals(5, person.getAccounts().size());
        for (int i = 0; i < 5; i++) {
            Assert.assertNotNull(person.getAccount(String.valueOf(i)));
        }
    }

    @Test
    public void addMoneyTest() throws RemoteException {
        Account account = bank.createAccount("moneyAccount");
        Assert.assertEquals(0, account.getAmount());
        for (int i = 1; i <= 5; i++) {
            account.addAmount(100);
            Assert.assertEquals(100 * i, account.getAmount());
        }
    }

    @Test
    public void localAndRemotePersonDifferenceTest() throws RemoteException {
        String passport = "222";
        bank.createPerson("Fedor", "Fedorov", passport);
        Person remotePerson = bank.getRemotePerson(passport);
        Person localPerson = bank.getLocalPerson(passport);
        bank.createPersonalAccount(remotePerson, "account");
        Assert.assertEquals(0, localPerson.getAccounts().size());
        Assert.assertEquals(1, remotePerson.getAccounts().size());
        localPerson = bank.getLocalPerson(passport);
        Assert.assertEquals(1, localPerson.getAccounts().size());
    }

    @Test
    public void addMoneyParallelTest() throws RemoteException, InterruptedException {
        Account account = bank.createAccount("parallelAccount");
        ExecutorService executor = Executors.newFixedThreadPool(5);
        for (int i = 0; i < 500000; i++) {
            executor.submit(() -> {
                try {
                    account.addAmount(1000);
                } catch (RemoteException e) {
                    System.err.println("Remote error occurred: " + e.getMessage());
                }
            });
        }
        executor.shutdown();
        executor.awaitTermination(1, TimeUnit.MINUTES);
        Assert.assertEquals(500000000, account.getAmount());
    }

    @Test
    public void personalAccountsParallelTest() throws RemoteException, InterruptedException {
        Person person = bank.createPerson("Anton", "Antonov", "111");
        List<Account> accounts = new ArrayList<>(5);
        ExecutorService executor = Executors.newFixedThreadPool(5);
        for (int i = 0; i < 5; i++) {
            final int index = i;
            executor.submit(() ->
                    accounts.set(index, bank.createPersonalAccount(person, String.valueOf(index))));
        }
        for (Account account : accounts) {
            for (int i = 0; i < 100000; i++) {
                executor.submit(() -> {
                    try {
                        account.addAmount(100);
                    } catch (RemoteException e) {
                        System.err.println("Remote error occurred: " + e.getMessage());
                    }
                });
            }
        }
        executor.shutdown();
        executor.awaitTermination(1, TimeUnit.MINUTES);
        Assert.assertEquals(5, person.getAccounts().size());
        for (Account account : accounts) {
            Assert.assertEquals(10000000, account.getAmount());
        }
    }
}
