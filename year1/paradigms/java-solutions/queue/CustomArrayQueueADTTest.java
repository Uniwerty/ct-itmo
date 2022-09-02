package queue;

class CustomArrayQueueADTTest {
    public static void main(String[] args) {
        ArrayQueueADT queue1 = ArrayQueueADT.create();
        ArrayQueueADT queue2 = ArrayQueueADT.create();
        for (int i = 0; i < 5; i++) {
            ArrayQueueADT.enqueue(queue1, "q1[" + i + "]");
            ArrayQueueADT.enqueue(queue2, "q2[" + i + "]");
        }
        System.out.println("Before clearing queue1: " + ArrayQueueADT.size(queue1) + " " + ArrayQueueADT.element(queue1));
        ArrayQueueADT.clear(queue1);
        System.out.println("After clearing queue1: " + ArrayQueueADT.size(queue1));
        System.out.println("Before clearing queue2: " + ArrayQueueADT.size(queue2) + " " + ArrayQueueADT.element(queue2));
        ArrayQueueADT.clear(queue2);
        System.out.println("After clearing queue2: " + ArrayQueueADT.size(queue2));
        System.out.println();

        for (int i = 0; i < 5; i++) {
            ArrayQueueADT.enqueue(queue1, "q1[" + i + "]");
            ArrayQueueADT.enqueue(queue2, "q2[" + i + "]");
        }
        dumpQueue(queue1);
        System.out.println();
        dumpQueue(queue2);
    }

    private static void dumpQueue(ArrayQueueADT queue) {
        while (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println(ArrayQueueADT.size(queue) + " " + ArrayQueueADT.dequeue(queue));
        }
    }
}