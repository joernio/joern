package io.joern.scanners.c

import io.joern.scanners.{Crew, QueryTags}
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*
import QueryLangExtensions.*

object SocketApi extends QueryBundle {

  @q
  def uncheckedSend()(implicit context: EngineContext): Query =
    Query.make(
      name = "socket-send",
      author = Crew.fabs,
      title = "Unchecked call to send",
      description = """
          | When calling `send`, the return value must be checked to determine
          | if the send operation was successful and how many bytes were
          | transmitted.
          |""".stripMargin,
      score = 2.0,
      withStrRep({ cpg =>
        implicit val noResolve: NoResolve.type = NoResolve
        cpg
          .method("send")
          .filter(_.parameter.size == 4)
          .callIn
          .returnValueNotChecked
      }),
      tags = List(QueryTags.default, QueryTags.posix),
      codeExamples = CodeExamples(
        List("""
          |
          |void return_not_checked(int sockfd, void *buf, size_t len, int flags) {
          |    send(sockfd, buf, len, flags);
          |}
          |
          |""".stripMargin),
        List(
          """
          |
          |void return_checked(int sockfd, void *buf, size_t len, int flags) {
          |    if (send(sockfd, buf, len, flags) <= 0) {
          |        // Do something
          |    }
          |}
          |
          |""".stripMargin,
          """
          |
          |void return_var_checked(int sockfd, void *buf, size_t len, int flags) {
          |    ssize_t ret = send(sockfd, buf, len, flags);
          |
          |    if (ret <= 0) {
          |        // Do something
          |    }
          |}
          |
          |""".stripMargin
        )
      )
    )
}
