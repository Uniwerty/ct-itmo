package homework03.data

data class TopicSnapshot(
    val creationTime: Int,
    val onlineUsers: Int,
    val description: String,
    val discussions: List<Discussion>,
    val receivingTime: Long
)