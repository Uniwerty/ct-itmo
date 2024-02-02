import java.util.concurrent.locks.ReentrantLock;

/**
 * Bank implementation.
 *
 * @author Ivchenkov Dmitrii
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    @Override
    public long getAmount(int index) {
        Account account = accounts[index];
        long amount;
        try {
            account.lock.lock();
            amount = account.amount;
        } finally {
            account.lock.unlock();
        }
        return amount;
    }

    @Override
    public long getTotalAmount() {
        long sum = 0;
        try {
            for (Account account : accounts) {
                account.lock.lock();
            }
            for (Account account : accounts) {
                sum += account.amount;
            }
        } finally {
            for (Account account : accounts) {
                account.lock.unlock();
            }
        }
        return sum;
    }

    @Override
    public long deposit(int index, long amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        Account account = accounts[index];
        long newAmount;
        try {
            account.lock.lock();
            if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT) {
                throw new IllegalStateException("Overflow");
            }
            newAmount = account.amount + amount;
            account.amount = newAmount;
        } finally {
            account.lock.unlock();
        }
        return newAmount;
    }

    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        Account account = accounts[index];
        long newAmount;
        try {
            account.lock.lock();
            if (account.amount - amount < 0) {
                throw new IllegalStateException("Underflow");
            }
            newAmount = account.amount - amount;
            account.amount = newAmount;
        } finally {
            account.lock.unlock();
        }
        return newAmount;
    }

    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        if (fromIndex == toIndex) {
            throw new IllegalArgumentException("fromIndex == toIndex");
        }
        Account from = accounts[fromIndex];
        Account to = accounts[toIndex];
        try {
            if (fromIndex < toIndex) {
                from.lock.lock();
                to.lock.lock();
            } else {
                to.lock.lock();
                from.lock.lock();
            }
            if (amount > from.amount) {
                throw new IllegalStateException("Underflow");
            } else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT) {
                throw new IllegalStateException("Overflow");
            }
            from.amount -= amount;
            to.amount += amount;
        } finally {
            from.lock.unlock();
            to.lock.unlock();
        }
    }

    /**
     * Private account data structure.
     */
    static class Account {
        /**
         * Amount of funds in this account.
         */
        long amount;
        /**
         * Account lock for thread-safety
         */
        ReentrantLock lock = new ReentrantLock();
    }
}
