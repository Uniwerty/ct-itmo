package homework03.data

data class Comment(
    val id: String,
    val replyTo: String,
    val depth: Int,
    val discussionId: String,
    val publicationTime: Int?,
    val ups: Int?,
    val downs: Int?,
    val text: String?,
    val author: String?,
    val replies: List<Comment>
)