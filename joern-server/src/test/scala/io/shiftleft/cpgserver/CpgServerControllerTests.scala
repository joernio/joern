package io.shiftleft.cpgserver

import akka.actor.ActorSystem
import org.json4s.JsonAST.JString
import org.json4s.native.JsonMethods._
import org.json4s.{JBool, _}
import org.scalatra.test.scalatest._

class CpgServerControllerTests extends ScalatraFunSuite {

  implicit val swagger = new CpgServerSwagger
  val system = ActorSystem()
  val impl = new NullServerImpl
  val controller = new CpgServerController(impl)
  addServlet(controller, "/*")

  def postJson[A](uri: String, body: JValue = JString(""), headers: Map[String, String] = Map())(f: => A): A =
    post(uri, compact(render(body)).getBytes("utf-8"), Map("Content-Type" -> "application/json") ++ headers)(f)

  test("string to create should give 400") {
    val filenames = JString("foo")
    postJson("/create", filenames) {
      status should equal(400)
    }
  }

  test("should respond with 400 when file does not exist") {
    val msg = JObject(JField("filenames", JArray(List(JString("foo")))))
    postJson("/create", msg) {
      status should equal(400)
    }
  }

  test("/status should return 200") {
    get("/status") {
      status should equal(200)
    }
  }

  test("/queryresult should return correct response") {
    get("/queryresult") {
      val response = parse(body)
      val isCompleted = for {
        JObject(child) <- response
        JField("isQueryCompleted", JBool(isQueryCompleted)) <- child
      } yield isQueryCompleted
      isCompleted.head shouldBe false
      status should equal(202)
    }
  }

}
