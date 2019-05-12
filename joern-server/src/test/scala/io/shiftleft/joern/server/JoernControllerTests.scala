package io.shiftleft.joern.server

import org.json4s._
import org.json4s.JsonAST.JString
import org.json4s.native.JsonMethods._
import org.scalatra.test.scalatest._

class JoernControllerTests extends ScalatraFunSuite {

  implicit val swagger = new JoernSwagger
  val controller = new JoernController
  addServlet(controller, "/*")

  def postJson[A](uri: String, body: JValue, headers: Map[String, String])(f: => A): A =
    post(uri, compact(render(body)).getBytes("utf-8"), Map("Content-Type" -> "application/json") ++ headers)(f)

  test("empty list to create should give 400") {
    val filenames = JArray(List())
    postJson("/create", filenames, Map()) {
      status should equal(400)
    }
  }

  test("string to create should give 400") {
    val filenames = JString("foo")
    postJson("/create", filenames, Map()) {
      status should equal(400)
    }
  }

}
