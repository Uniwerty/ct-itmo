package homework03.data

data class Discussion(
    val id: String,
    val author: String,
    val publicationTime: Int,
    val ups: Int,
    val downs: Int,
    val title: String,
    val text: String,
    val htmlText: String
)