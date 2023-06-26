package info.kgeorgiy.ja.ivchenkov.bank;

import java.util.concurrent.ConcurrentMap;

/**
 * The local person class
 *
 * @author Ivchenkov Dmitrii
 */
public class LocalPerson extends AbstractPerson {
    protected LocalPerson(String name, String surname, String passport, ConcurrentMap<String, Account> accounts) {
        super(name, surname, passport, accounts);
    }
}
