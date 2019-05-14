package io.shiftleft.joern.server

import akka.actor.ActorSystem
import org.json4s._
import org.json4s.JsonAST.JString
import org.json4s.native.JsonMethods._
import org.scalatra.test.scalatest._

class JoernControllerTests extends ScalatraFunSuite {

  implicit val swagger = new JoernSwagger
  val system = ActorSystem()
  val controller = new JoernController(system)
  addServlet(controller, "/*")

  def postJson[A](uri: String, body: JValue, headers: Map[String, String])(f: => A): A =
    post(uri, compact(render(body)).getBytes("utf-8"), Map("Content-Type" -> "application/json") ++ headers)(f)

  test("string to create should give 400") {
    val filenames = JString("foo")
    postJson("/create", filenames, Map()) {
      status should equal(400)
    }
  }

  test("should respond with 400 when file does not exist") {
    val msg = JObject(JField("filenames", JArray(List(JString("foo")))))
    postJson("/create", msg, Map()) {
      status should equal(400)
    }
  }

}
