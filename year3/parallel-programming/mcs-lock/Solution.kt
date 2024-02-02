import java.util.concurrent.atomic.*

/**
 * @author Ivchenkov Dmitrii
 */
class Solution(val env: Environment) : Lock<Solution.Node> {
    val tail = AtomicReference<Node?>(null)

    override fun lock(): Node {
        val my = Node()
        val prev = tail.getAndSet(my)
        if (prev != null) {
            prev.next.value = my
            while (my.locked.value) {
                env.park()
            }
        }
        return my
    }

    override fun unlock(node: Node) {
        if (node.next.value == null) {
            if (tail.compareAndSet(node, null)) {
                return
            } else {
                while (true) {
                    if (node.next.value != null) {
                        break
                    }
                }
            }
        }
        node.next.value!!.locked.value = false
        env.unpark(node.next.value!!.thread)
    }

    class Node {
        val thread = Thread.currentThread()
        val next = AtomicReference<Node?>(null)
        val locked = AtomicReference(true)
    }
}