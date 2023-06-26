package homework03.data

data class CommentsSnapshot(
    val receivingTime: Long,
    val comments: List<Comment>,
    val linearComments: List<Comment>
)