package info.kgeorgiy.ja.ivchenkov.bank;

/**
 * The abstract class implementing {@link Account}
 *
 * @author Ivchenkov Dmitrii
 */
public abstract class AbstractAccount implements Account {
    private final String id;
    private long amount;

    protected AbstractAccount(final String id, final long amount) {
        this.id = id;
        this.amount = amount;
    }

    protected AbstractAccount(final String id) {
        this(id, 0);
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized long getAmount() {
//        System.out.printf("Getting amount of money for account %s%n", id);
        return amount;
    }

    @Override
    public synchronized void setAmount(final long amount) {
//        System.out.printf("Setting amount of money to %d for account %s%n", amount, id);
        this.amount = amount;
    }

    @Override
    public synchronized void addAmount(final long amount) {
//        System.out.printf("Adding %d for account %s%n", amount, id);
        this.amount += amount;
    }
}
