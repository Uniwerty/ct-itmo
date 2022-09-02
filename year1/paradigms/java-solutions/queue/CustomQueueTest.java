package queue;

class CustomQueueTest {
    public static void main(String[] args) {
        Queue queue1 = new ArrayQueue();
        Queue queue2 = new LinkedQueue();
        for (int i = 0; i < 5; i++) {
            queue1.enqueue("q1[" + i + "]");
            queue2.enqueue("q2[" + i + "]");
        }
        System.out.println("Before clearing queue1: " + queue1.size() + " " + queue1.element());
        queue1.clear();
        System.out.println("After clearing queue1: " + queue1.size());
        System.out.println("Before clearing queue2: " + queue2.size() + " " + queue2.element());
        queue2.clear();
        System.out.println("After clearing queue2: " + queue2.size());
        System.out.println();

        for (int i = 0; i < 5; i++) {
            queue1.enqueue("q1[" + i + "]");
            queue2.enqueue("q2[" + i + "]");
        }
        dumpQueue(queue1);
        System.out.println();
        dumpQueue(queue2);
    }

    private static void dumpQueue(Queue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " + queue.dequeue());
        }
    }
}