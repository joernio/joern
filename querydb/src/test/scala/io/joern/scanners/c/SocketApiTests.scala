package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal

class SocketApiTests extends CQueryTestSuite {

  override def queryBundle = SocketApi

  override val code: String =
    """
      |void return_not_checked(int sockfd, void *buf, size_t len, int flags) {
      |    send(sockfd, buf, len, flags);
      |}
      |
      |void return_checked(int sockfd, void *buf, size_t len, int flags) {
      |    if (send(sockfd, buf, len, flags) <= 0) {
      |        // Do something
      |    }
      |}
      |
      |void return_var_checked(int sockfd, void *buf, size_t len, int flags) {
      |    ssize_t ret = send(sockfd, buf, len, flags);
      |
      |    if (ret <= 0) {
      |        // Do something
      |    }
      |}
      |""".stripMargin

  "should flag function `return_not_checked` only" in {
    val query   = queryBundle.uncheckedSend()
    val results = findMatchingCalls(query)

    results shouldBe Set("return_not_checked")
  }

}
