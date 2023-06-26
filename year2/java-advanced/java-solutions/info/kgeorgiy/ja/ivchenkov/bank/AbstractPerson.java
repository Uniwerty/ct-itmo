package info.kgeorgiy.ja.ivchenkov.bank;

import java.io.Serializable;
import java.util.concurrent.ConcurrentMap;

/**
 * The abstract class implementing {@link Person}
 *
 * @author Ivchenkov Dmitrii
 */
public abstract class AbstractPerson implements Person, Serializable {
    private final String name;
    private final String surname;
    private final String passport;
    private final ConcurrentMap<String, Account> accounts;

    protected AbstractPerson(String name, String surname, String passport, ConcurrentMap<String, Account> accounts) {
        this.name = name;
        this.surname = surname;
        this.passport = passport;
        this.accounts = accounts;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getSurname() {
        return surname;
    }

    @Override
    public String getPassport() {
        return passport;
    }

    @Override
    public ConcurrentMap<String, Account> getAccounts() {
        return accounts;
    }

    @Override
    public Account getAccount(final String subId) {
        return accounts.get(getAccountId(subId));
    }

    @Override
    public String getAccountId(final String subId) {
        return String.format("%s:%s", passport, subId);
    }

    @Override
    public void addAccount(final String subId, final Account account) {
        accounts.putIfAbsent(getAccountId(subId), account);
    }
}
