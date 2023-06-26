package homework03.data

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.http.*
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import java.lang.RuntimeException
import kotlin.collections.ArrayList

class RedditClient {
    private val httpClient = HttpClient()
    private val mapper = jacksonObjectMapper()
    private val redditUrl = "https://www.reddit.com/r/"

    suspend fun getTopic(name: String): TopicSnapshot {
        var aboutTopicResponse = ""
        var mainPageResponse = ""
        coroutineScope {
            launch { aboutTopicResponse = request("$redditUrl$name/about/.json") }
            launch { mainPageResponse = request("$redditUrl$name/.json") }
        }
        val receivingTime = System.currentTimeMillis()

        val mapper = jacksonObjectMapper()
        val aboutTopicNode =
            mapper.readTree(aboutTopicResponse) ?: throw RedditClientException.JSONParsingException(name)
        val mainPageNode = mapper.readTree(mainPageResponse) ?: throw RedditClientException.JSONParsingException(name)

        val discussionList =
            mainPageNode.get("data").get("children") ?: throw RedditClientException.JSONParsingException(name)
        val discussions = ArrayList<Discussion>()
        for (node in discussionList) {
            val discussionData = node.get("data") ?: throw RedditClientException.JSONParsingException(name)
            discussions.add(
                Discussion(
                    id = discussionData.get("id").asText(),
                    author = discussionData.get("author").asText(),
                    publicationTime = discussionData.get("created").asInt(),
                    ups = discussionData.get("ups").asInt(),
                    downs = discussionData.get("downs").asInt(),
                    title = discussionData.get("title").asText(),
                    text = discussionData.get("selftext").asText(),
                    htmlText = discussionData.get("selftext_html").asText()
                )
            )
        }
        val topicData = aboutTopicNode.get("data") ?: throw RedditClientException.JSONParsingException(name)
        return TopicSnapshot(
            creationTime = topicData.get("created").asInt(),
            onlineUsers = topicData.get("active_user_count").asInt(),
            description = topicData.get("public_description").asText(),
            discussions = discussions,
            receivingTime = receivingTime
        )
    }

    suspend fun getComments(topic: String, discussionId: String): CommentsSnapshot {
        val response = request("$redditUrl$topic/comments/$discussionId/.json")
        val receivingTime = System.currentTimeMillis()

        val commentsNode = mapper.readTree(response) ?: throw RedditClientException.JSONParsingException(topic)
        val comments = getCommentsList(commentsNode.get(1).get("data").get("children"), discussionId)
        val linearComments = getLinearCommentsList(comments)
        return CommentsSnapshot(receivingTime, comments, linearComments)
    }

    private suspend fun request(url: String): String {
        val response = httpClient.get(url)
        if (!response.status.isSuccess()) {
            throw RedditClientException.RequestException(url)
        }
        return response.body()
    }

    private fun getCommentsList(commentsNode: JsonNode?, discussionId: String, depth: Int = 1): List<Comment> {
        val comments = ArrayList<Comment>()
        if (commentsNode != null) {
            for (node in commentsNode) {
                comments.add(
                    Comment(
                        id = node.get("data").get("id").asText(),
                        replyTo = node.get("data").get("parent_id").asText(),
                        depth = depth,
                        discussionId = discussionId,
                        publicationTime = node.get("data").get("created")?.asInt(),
                        ups = node.get("data").get("ups")?.asInt(),
                        downs = node.get("data").get("downs")?.asInt(),
                        text = node.get("data").get("body")?.asText(),
                        author = node.get("data").get("author")?.asText(),
                        replies = getCommentsList(
                            node.get("data").get("replies")?.get("data")?.get("children"),
                            discussionId,
                            depth + 1
                        )
                    )
                )
            }
        }
        return comments
    }

    private fun getLinearCommentsList(comments: List<Comment>): List<Comment> {
        val linearComments = ArrayList<Comment>()
        for (comment in comments) {
            linearComments.add(comment)
            linearComments.addAll(getLinearCommentsList(comment.replies))
        }
        return linearComments
    }

    sealed class RedditClientException(reason: String) : RuntimeException(reason) {
        class RequestException(url: String) : RedditClientException("An error occurred while requesting $url")
        class JSONParsingException(topic: String) :
            RedditClientException("An error occurred while parsing $topic JSON file")
    }
}
