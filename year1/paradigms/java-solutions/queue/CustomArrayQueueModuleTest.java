package queue;

class CustomArrayQueueModuleTest {
    public static void main(String[] args) {
        for (int i = 0; i < 5; i++) {
            ArrayQueueModule.enqueue("e" + i);
        }
        System.out.println("Before clearing: " + ArrayQueueModule.size() + " " + ArrayQueueModule.element());
        ArrayQueueModule.clear();
        System.out.println("After clearing: " + ArrayQueueModule.size());
        System.out.println();

        for (int i = 0; i < 5; i++) {
            ArrayQueueModule.enqueue("e" + i);
        }
        while (!ArrayQueueModule.isEmpty()) {
            System.out.println(ArrayQueueModule.size() + " " + ArrayQueueModule.dequeue());
        }
    }
}