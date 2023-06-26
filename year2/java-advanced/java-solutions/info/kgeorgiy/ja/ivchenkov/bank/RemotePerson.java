package info.kgeorgiy.ja.ivchenkov.bank;

import java.util.concurrent.ConcurrentMap;

/**
 * The remote person class
 *
 * @author Ivchenkov Dmitrii
 */
public class RemotePerson extends AbstractPerson {
    public RemotePerson(String name, String surname, String passport, ConcurrentMap<String, Account> accounts) {
        super(name, surname, passport, accounts);
    }
}
