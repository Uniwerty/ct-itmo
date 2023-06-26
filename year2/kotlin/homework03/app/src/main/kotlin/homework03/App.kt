package homework03

import com.soywiz.korio.file.VfsOpenMode
import com.soywiz.korio.file.std.localCurrentDirVfs
import com.soywiz.korio.stream.writeString
import homework03.data.Comment
import homework03.data.Discussion
import homework03.data.RedditClient
import kotlinx.coroutines.CoroutineExceptionHandler
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.supervisorScope

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println("No arguments given")
        return
    }
    val client = RedditClient()
    val cwd = localCurrentDirVfs
    runBlocking {
        for (topic in args) {
            val exceptionHandler = CoroutineExceptionHandler { context, exception ->
                System.err.println("Caught while working with $topic: ${exception.message}")
            }
            launch(exceptionHandler) {
                val topicSnapshot = client.getTopic(topic)
                launch {
                    val topicFile = cwd["$topic-subjects.csv"].open(VfsOpenMode.WRITE)
                    topicFile.writeString(csvSerializeHeader(Discussion::class))
                    topicFile.writeString(csvSerialize(topicSnapshot.discussions, Discussion::class))
                    topicFile.close()
                }
                launch {
                    val commentsFile = cwd["$topic-comments.csv"].open(VfsOpenMode.WRITE)
                    commentsFile.writeString(csvSerializeHeader(Comment::class))
                    supervisorScope {
                        for (discussion in topicSnapshot.discussions) {
                            launch {
                                val commentsSnapshot = client.getComments(topic, discussion.id)
                                commentsFile.writeString(csvSerialize(commentsSnapshot.linearComments, Comment::class))
                            }
                        }
                    }
                    commentsFile.close()
                }
            }
        }
    }
}