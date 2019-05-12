package io.shiftleft.joern.server

import org.scalatra.test.scalatest._

class JoernServletTests extends ScalatraFunSuite {

  addServlet(classOf[JoernServlet], "/*")

  test("GET / on JoernServlet should return status 200") {
    get("/") {
      status should equal (200)
    }
  }

}
