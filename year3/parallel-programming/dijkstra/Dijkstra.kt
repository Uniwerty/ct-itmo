package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.ReentrantLock
import kotlin.Comparator
import kotlin.collections.ArrayList
import kotlin.concurrent.thread

/**
 * @author Ivchenkov Dmitrii
 */

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val queue = MultiQueue(workers * 2)
    queue.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val active = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (active.get() > 0) {
                val cur = queue.poll() ?: continue
                for (edge in cur.outgoingEdges) {
                    val distance = cur.distance + edge.weight
                    while (true) {
                        val curDistance = edge.to.distance
                        if (distance < curDistance) {
                            if (edge.to.casDistance(curDistance, distance)) {
                                active.getAndIncrement()
                                queue.add(edge.to)
                                break
                            }
                        } else {
                            break
                        }
                    }
                }
                active.getAndDecrement()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

class MultiQueue(val capacity: Int) {
    private val queues: ArrayList<PriorityQueue<Node>> = ArrayList(capacity)
    private val locks: ArrayList<ReentrantLock> = ArrayList(capacity)

    init {
        for (i in 0 until capacity) {
            queues.add(PriorityQueue(NODE_DISTANCE_COMPARATOR))
            locks.add(ReentrantLock())
        }
    }

    fun add(node: Node) {
        val random = Random()
        while (true) {
            val i = random.nextInt(capacity)
            val lock = locks[i]
            if (lock.tryLock()) {
                try {
                    queues[i].add(node)
                    return
                } finally {
                    lock.unlock()
                }
            }
        }
    }

    fun poll(): Node? {
        val random = Random()
        while (true) {
            val i1 = random.nextInt(capacity)
            val i2 = random.nextInt(capacity)
            if (i1 == i2) continue
            val lock1 = locks[i1]
            if (lock1.tryLock()) {
                try {
                    val lock2 = locks[i2]
                    if (lock2.tryLock()) {
                        try {
                            val queue1 = queues[i1]
                            val queue2 = queues[i2]
                            val node1 = queue1.peek()
                            val node2 = queue2.peek()
                            val result = if (node1 != null && node2 != null) {
                                if (NODE_DISTANCE_COMPARATOR.compare(node1, node2) < 0) {
                                    queue1.poll()
                                } else {
                                    queue2.poll()
                                }
                            } else if (node1 == null && node2 == null) {
                                null
                            } else if (node1 != null) {
                                queue1.poll()
                            } else {
                                queue2.poll()
                            }
                            return result
                        } finally {
                            lock2.unlock()
                        }
                    }
                } finally {
                    lock1.unlock()
                }
            }
        }
    }
}