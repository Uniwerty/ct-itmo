package info.kgeorgiy.ja.ivchenkov.bank;

/**
 * The local account class
 *
 * @author Ivchenkov Dmitrii
 */
public class LocalAccount extends AbstractAccount {
    public LocalAccount(String id) {
        super(id);
    }

    public LocalAccount(String id, long amount) {
        super(id, amount);
    }
}
