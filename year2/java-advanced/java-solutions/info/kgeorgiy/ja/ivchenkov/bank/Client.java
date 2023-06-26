package info.kgeorgiy.ja.ivchenkov.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

/**
 * The client application class
 *
 * @author Ivchenkov Dmitrii
 */
public final class Client {
    private Client() {
    }

    public static void main(final String[] args) {
        if (args == null || args.length != 5) {
            System.err.println("Invalid arguments number! Please write: name surname passport account amount");
            return;
        }
        final String name = args[0];
        final String surname = args[1];
        final String passport = args[2];
        final int additionAmount;
        try {
            additionAmount = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {
            System.err.println("Amount of money must be an integer!");
            return;
        }

        try {
            final Bank bank;
            try {
                bank = (Bank) Naming.lookup("//localhost/bank");
            } catch (final NotBoundException e) {
                System.err.println("Bank is not bound");
                return;
            } catch (final MalformedURLException e) {
                System.err.println("Bank URL is invalid");
                return;
            }

            Person person = bank.getLocalPerson(passport);
            if (person == null) {
                person = bank.createPerson(name, surname, passport);
            } else if (!name.equals(person.getName()) || !surname.equals(person.getSurname())) {
                System.err.println("Person not verified: the data does not match the passport!");
                return;
            } else {
                System.out.println("Person verified");
            }

            final String accountId = person.getAccountId(args[3]);
            Account account = bank.getAccount(accountId);
            if (account == null) {
                account = bank.createAccount(accountId);
                person.addAccount(args[3], account);
            }

            System.out.printf("Account id: %s%n", accountId);
            System.out.printf("Money: %d%n", account.getAmount());
            System.out.println("Adding money");
            final long newAmount = account.getAmount() + additionAmount;
            account.setAmount(newAmount);
            System.out.printf("Money: %d%n", newAmount);
        } catch (RemoteException e) {
            System.err.println("Remote error occurred: " + e.getMessage());
        }
    }
}
