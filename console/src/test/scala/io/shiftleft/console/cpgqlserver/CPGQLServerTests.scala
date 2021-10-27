package io.shiftleft.console.cpgqlserver

import cask.util.Logger.Console._
import castor.Context.Simple.global
import io.shiftleft.console.embammonite.EmbeddedAmmonite
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import requests.RequestFailedException
import ujson.Value.Value

import java.net.URLEncoder
import java.util.UUID
import java.util.concurrent.locks.{Lock, ReentrantLock}
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._

class CPGQLServerTests extends AnyWordSpec with Matchers {
  val validBasicAuthHeaderVal = "Basic dXNlcm5hbWU6cGFzc3dvcmQ="

  val DefaultPromiseAwaitTimeout = Duration(10, SECONDS)

  def postQuery(host: String, query: String, authHeaderVal: String = validBasicAuthHeaderVal): Value = {
    val postResponse = requests.post(s"$host/query",
                                     data = ujson.Obj("query" -> query).toString,
                                     headers = Seq("authorization" -> authHeaderVal))
    val res =
      if (postResponse.bytes.length > 0)
        ujson.read(postResponse.bytes)
      else
        ujson.Obj()
    res
  }

  def getResponse(host: String, uuidParam: String, authHeaderVal: String = validBasicAuthHeaderVal): Value = {
    val uri = s"$host/result/${URLEncoder.encode(uuidParam, "utf-8")}"
    val getResponse = requests.get(uri, headers = Seq("authorization" -> authHeaderVal))
    ujson.read(getResponse.bytes)
  }

  "CPGQLServer" should {

    "allow websocket connections to the `/connect` endpoint" in Fixture() { host =>
      val wsMsgPromise = scala.concurrent.Promise[String]()
      cask.util.WsClient.connect(s"$host/connect") {
        case cask.Ws.Text(msg) => wsMsgPromise.success(msg)
      }
      val wsMsg = Await.result(wsMsgPromise.future, DefaultPromiseAwaitTimeout)
      wsMsg shouldBe "connected"
    }

    "allow posting a simple query without any websocket connections established" in Fixture() { host =>
      val postQueryResponse = postQuery(host, "1")
      postQueryResponse.obj.keySet should contain("success")
      val UUIDResponse = postQueryResponse("uuid").str
      UUIDResponse should not be empty
      postQueryResponse("success").bool shouldBe true
    }

    "disallow posting a query when request headers do not include a valid authentication value" in Fixture() { host =>
      assertThrows[RequestFailedException] {
        postQuery(host, "1", authHeaderVal = "Basic b4df00d")
      }
    }

    "return a valid JSON response when trying to retrieve the result of a query without a connection" in Fixture() {
      host =>
        val postQueryResponse = postQuery(host, "1")
        postQueryResponse.obj.keySet should contain("uuid")
        val UUIDResponse = postQueryResponse("uuid").str
        val getResultResponse = getResponse(host, UUIDResponse)
        getResultResponse.obj.keySet should contain("success")
        getResultResponse.obj.keySet should contain("err")
        getResultResponse("success").bool shouldBe false
        getResultResponse("err").str.length should not be (0)
    }

    "allow fetching the result of a completed query using its UUID" in Fixture() { host =>
      val wsMsgPromise = scala.concurrent.Promise[String]()
      val connectedPromise = scala.concurrent.Promise[String]()
      cask.util.WsClient.connect(s"$host/connect") {
        case cask.Ws.Text(msg) => {
          if (msg == "connected") {
            connectedPromise.success(msg)
          } else {
            wsMsgPromise.success(msg)
          }
        }
      }
      Await.result(connectedPromise.future, DefaultPromiseAwaitTimeout)
      val postQueryResponse = postQuery(host, "1")
      val queryUUID = postQueryResponse("uuid").str
      queryUUID.length should not be (0)

      val queryResultWSMessage = Await.result(wsMsgPromise.future, DefaultPromiseAwaitTimeout)
      queryResultWSMessage.length should not be (0)

      val getResultResponse = getResponse(host, queryUUID)
      getResultResponse.obj.keySet should contain("success")
      getResultResponse("uuid").str shouldBe queryResultWSMessage
      getResultResponse("stdout").str shouldBe "res0: Int = 1\n"
      getResultResponse("stderr").str shouldBe ""
    }

    "disallow fetching the result of a completed query with an invalid auth header" in Fixture() { host =>
      val wsMsgPromise = scala.concurrent.Promise[String]()
      val connectedPromise = scala.concurrent.Promise[String]()
      cask.util.WsClient.connect(s"$host/connect") {
        case cask.Ws.Text(msg) => {
          if (msg == "connected") {
            connectedPromise.success(msg)
          } else {
            wsMsgPromise.success(msg)
          }
        }
      }
      Await.result(connectedPromise.future, DefaultPromiseAwaitTimeout)
      val postQueryResponse = postQuery(host, "1")
      val queryUUID = postQueryResponse("uuid").str
      queryUUID.length should not be (0)

      val queryResultWSMessage = Await.result(wsMsgPromise.future, DefaultPromiseAwaitTimeout)
      queryResultWSMessage.length should not be (0)

      assertThrows[RequestFailedException] {
        getResponse(host, queryUUID, "Basic b4df00d")
      }
    }

    "write a well-formatted message to a websocket connection when a query has finished evaluation" in Fixture() {
      host =>
        val wsMsgPromise = scala.concurrent.Promise[String]()
        val connectPromise = scala.concurrent.Promise[String]()
        cask.util.WsClient.connect(s"$host/connect") {
          case cask.Ws.Text(msg) => {
            if (msg == "connected") {
              connectPromise.success(msg)
            } else {
              wsMsgPromise.success(msg)
            }
          }
        }
        Await.result(connectPromise.future, DefaultPromiseAwaitTimeout)

        val postQueryResponse = postQuery(host, "1")
        val queryUUID = postQueryResponse("uuid").str
        queryUUID.length should not be (0)

        val queryResultWSMessage = Await.result(wsMsgPromise.future, DefaultPromiseAwaitTimeout)
        queryResultWSMessage.length should not be (0)

        val getResultResponse = getResponse(host, queryUUID)
        getResultResponse.obj.keySet should contain("success")
        getResultResponse.obj.keySet should contain("stdout")
        getResultResponse.obj.keySet should contain("stderr")
        getResultResponse.obj.keySet should not contain ("err")
        getResultResponse("uuid").str shouldBe queryResultWSMessage
        getResultResponse("stdout").str shouldBe "res0: Int = 1\n"
        getResultResponse("stderr").str shouldBe ""
    }

    "write a well-formatted message to a websocket connection when a query failed evaluation" in Fixture() { host =>
      val wsMsgPromise = scala.concurrent.Promise[String]()
      val connectedPromise = scala.concurrent.Promise[String]()
      cask.util.WsClient.connect(s"$host/connect") {
        case cask.Ws.Text(msg) => {
          if (msg == "connected") {
            connectedPromise.success(msg)
          } else {
            wsMsgPromise.success(msg)
          }
        }
      }
      Await.result(connectedPromise.future, DefaultPromiseAwaitTimeout)

      val postQueryResponse = postQuery(host, "if else for loop soup // i.e., an invalid Ammonite query")
      val queryUUID = postQueryResponse("uuid").str
      queryUUID.length should not be (0)

      val wsMsg = Await.result(wsMsgPromise.future, DefaultPromiseAwaitTimeout)
      wsMsg.length should not be (0)

      val resp = getResponse(host, queryUUID)
      resp.obj.keySet should contain("success")
      resp.obj.keySet should contain("stdout")
      resp.obj.keySet should contain("stderr")
      resp.obj.keySet should not contain ("err")

      resp("success").bool shouldBe false
      resp("uuid").str shouldBe wsMsg
      resp("stdout").str shouldBe ""
      resp("stderr").str.length should not be (0)
    }

    "write a well-formatted message to a websocket connection when a query containing an invalid char is submitted" in Fixture() {
      host =>
        val wsMsgPromise = scala.concurrent.Promise[String]()
        val connectedPromise = scala.concurrent.Promise[String]()
        cask.util.WsClient.connect(s"$host/connect") {
          case cask.Ws.Text(msg) => {
            if (msg == "connected") {
              connectedPromise.success(msg)
            } else {
              wsMsgPromise.success(msg)
            }
          }
        }
        Await.result(connectedPromise.future, DefaultPromiseAwaitTimeout)

        val postQueryResponse = postQuery(host, "@1")
        val queryUUID = postQueryResponse("uuid").str
        queryUUID.length should not be (0)

        val wsMsg = Await.result(wsMsgPromise.future, DefaultPromiseAwaitTimeout)
        wsMsg.length should not be (0)

        val resp = getResponse(host, queryUUID)
        resp.obj.keySet should contain("success")
        resp.obj.keySet should contain("stdout")
        resp.obj.keySet should contain("stderr")
        resp.obj.keySet should not contain ("err")

        resp("success").bool shouldBe false
        resp("uuid").str shouldBe wsMsg
        resp("stdout").str shouldBe ""
        resp("stderr").str.length should not be (0)
    }
  }

  "receive error when attempting to retrieve result with invalid uuid" in Fixture() { host =>
    val connectedPromise = scala.concurrent.Promise[String]()
    cask.util.WsClient.connect(s"$host/connect") {
      case cask.Ws.Text(msg) => connectedPromise.success(msg)
    }
    Await.result(connectedPromise.future, Duration(1, SECONDS))
    val getResultResponse = getResponse(host, UUID.randomUUID().toString)
    getResultResponse.obj.keySet should contain("success")
    getResultResponse.obj.keySet should contain("err")
    getResultResponse("success").bool shouldBe false
  }

  "return a valid JSON response when calling /result with incorrectly-formatted UUID parameter" in Fixture() { host =>
    val connectedPromise = scala.concurrent.Promise[String]()
    cask.util.WsClient.connect(s"$host/connect") {
      case cask.Ws.Text(msg) => connectedPromise.success(msg)
    }
    Await.result(connectedPromise.future, Duration(1, SECONDS))
    val getResultResponse = getResponse(host, "INCORRECTLY_FORMATTED_UUID_PARAM")
    getResultResponse.obj.keySet should contain("success")
    getResultResponse.obj.keySet should contain("err")
    getResultResponse("success").bool shouldBe false
    getResultResponse("err").str.length should not equal (0)
  }

  "return websocket responses for all queries when posted quickly in a large number" in Fixture() { host =>
    val numQueries = 10
    val correctNumberOfUUIDsReceived = scala.concurrent.Promise[String]()
    val wsUUIDs = ListBuffer[String]()

    val rtl: Lock = new ReentrantLock()
    val connectedPromise = scala.concurrent.Promise[String]()
    cask.util.WsClient.connect(s"$host/connect") {
      case cask.Ws.Text(msg) => {
        if (msg == "connected") {
          connectedPromise.success(msg)
        } else {
          rtl.lock()
          try {
            wsUUIDs += msg
          } finally {
            rtl.unlock()
            if (wsUUIDs.size == numQueries) {
              correctNumberOfUUIDsReceived.success("")
            }
          }
        }
      }
    }
    Await.result(connectedPromise.future, DefaultPromiseAwaitTimeout)

    val postQueriesResponseUUIDs =
      for (_ <- 1 to numQueries) yield {
        val postQueryResponse = postQuery(host, "1")
        postQueryResponse("uuid").str
      }

    Await.result(correctNumberOfUUIDsReceived.future, DefaultPromiseAwaitTimeout * numQueries.toLong)
    wsUUIDs.toSet should be(postQueriesResponseUUIDs.toSet)
  }

  "return websocket responses for all queries when some are invalid" in Fixture() { host =>
    val queries = List("1", "1 + 1", "open(", "open)", "open{", "open}")
    val correctNumberOfUUIDsReceived = scala.concurrent.Promise[String]()
    val wsUUIDs = ListBuffer[String]()
    val connectedPromise = scala.concurrent.Promise[String]()

    val rtl: Lock = new ReentrantLock()
    cask.util.WsClient.connect(s"$host/connect") {
      case cask.Ws.Text(msg) => {
        if (msg == "connected") {
          connectedPromise.success(msg)
        } else {
          rtl.lock()
          try {
            wsUUIDs += msg
          } finally {
            rtl.unlock()
            if (wsUUIDs.size == queries.size) {
              correctNumberOfUUIDsReceived.success("")
            }
          }
        }
      }
    }
    Await.result(connectedPromise.future, DefaultPromiseAwaitTimeout)

    val postQueriesResponseUUIDs = {
      queries
        .map(q => {
          val res = postQuery(host, q)
          res("uuid").str
        })
    }
    Await.result(correctNumberOfUUIDsReceived.future, DefaultPromiseAwaitTimeout * queries.size.toLong)
    wsUUIDs.toSet should be(postQueriesResponseUUIDs.toSet)
  }
}

object Fixture {

  def apply[T]()(f: String => T): T = {
    val ammonite = new EmbeddedAmmonite()
    ammonite.start()

    val host = "localhost"
    val port = 8081
    val authUsername = "username"
    val authPassword = "password"
    val httpEndpoint = "http://" + host + ":" + port.toString()
    val ammServer = new CPGQLServer(ammonite, host, port, authUsername, authPassword)
    val server = io.undertow.Undertow.builder
      .addHttpListener(ammServer.port, ammServer.host)
      .setHandler(ammServer.defaultHandler)
      .build
    server.start()
    val res =
      try { f(httpEndpoint) } finally {
        server.stop()
        ammonite.shutdown()
      }
    res
  }
}
