package io.shiftleft.joern.server

import org.scalatra.test.scalatest._

class JoernControllerTests extends ScalatraFunSuite {

  addServlet(classOf[JoernController], "/*")

//  test("GET / on JoernServlet should return status 200") {
//    get("/") {
//      status should equal (200)
//    }
//  }

}
