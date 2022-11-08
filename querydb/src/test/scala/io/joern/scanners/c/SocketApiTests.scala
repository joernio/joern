package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.joern.x2cpg.testfixtures.TestCpg

class SocketApiTests extends CQueryTestSuite(SocketApi) {

  override val cpg: TestCpg = code("""
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
      |""".stripMargin)

  "should flag function `return_not_checked` only" in {
    val query   = queryBundle.uncheckedSend()
    val results = findMatchingCalls(query)
    results shouldBe Set("return_not_checked")
  }

}
