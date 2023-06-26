package info.kgeorgiy.ja.ivchenkov.concurrent;

import java.util.List;

public class Util {
    public static void join(List<Thread> threadsList) throws InterruptedException {
        final int parts = threadsList.size();
        InterruptedException exception;
        for (int i = 0; i < parts; i++) {
            try {
                threadsList.get(i).join();
            } catch (InterruptedException e) {
                exception = e;
                for (int j = i + 1; j < parts; j++) {
                    threadsList.get(j).interrupt();
                }
                int j = i;
                while (j < parts) {
                    try {
                        threadsList.get(j).join();
                    } catch (InterruptedException ex) {
                        e.addSuppressed(ex);
                        j--;
                    }
                    j++;
                }
                throw exception;
            }
        }
    }
}
