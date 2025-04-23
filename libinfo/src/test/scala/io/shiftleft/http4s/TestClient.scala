package io.shiftleft.http4s

import cats.ApplicativeError
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.comcast.ip4s.{Host, Port, ipv4, port}
import fs2.io.net.BindException
import fs2.{Pipe, Stream}
import io.shiftleft.resolver.impl.{CoordinateConverterIon, IdConverterIonMaven, MetaDataCalculatorRemote}
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.{HttpApp, HttpRoutes, Uri, websocket}
import org.http4s.client.websocket.WSRequest
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.server.websocket.WebSocketBuilder2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.http4s.client.websocket.WSFrame.Text
import org.http4s.websocket.WebSocketFrame

class WSClientImplTests extends AnyWordSpec with Matchers {
  def createWSServer(host: Host, port: Int, recvPipe: Pipe[IO, WebSocketFrame, Unit])
                    (implicit F: ApplicativeError[IO, Throwable]): Resource[IO, Server] = {
    val server = EmberServerBuilder
      .default[IO]
      .withHost(host)
      .withPort(Port.fromInt(port).get)
      .withHttpWebSocketApp(createWSHttpApp(recvPipe))
      .build

    server.handleErrorWith { (error: Throwable) =>
      createWSServer(host, port + 1, recvPipe)
    }
  }

  def createWSHttpApp(recvPipe: Pipe[IO, WebSocketFrame, Unit])(builder: WebSocketBuilder2[IO]): HttpApp[IO] = {
    val service = HttpRoutes.of[IO] {
      case GET -> Root / "test" =>
        builder.build(Stream.empty, recvPipe)
    }
    Router.apply[IO]("/" -> service).orNotFound
  }

  def runServer[A](recvPipe: Pipe[IO, WebSocketFrame, Unit])(testFunc: (Server, Uri) => IO[A]): Unit = {
    new IOApp {
      // Currently one of the htt4ps libraries causes unsuccessful port binding attempts
      // to be logged if we do not override the report failure handling.
      // This is in contrast to the normal exception propagation and i consider it a bug.
      override def reportFailure(err: Throwable): IO[Unit] = {
        err match {
          case _: BindException =>
            IO.unit
          case _ =>
            IO.blocking(err.printStackTrace())
        }
      }
      override def run(args: List[String]): IO[ExitCode] = {
        val host = ipv4"127.0.0.1"
        createWSServer(host, 1024, recvPipe).use { server =>
          val port = server.address.getPort
          val uri = Uri.fromString(s"ws://$host:$port/test").toOption.get
          testFunc(server, uri).as(ExitCode.Success)
        }

      }
    }.main(Array.empty)
  }

  "Client should be able to connect" in {
    var connectionEstablished = false
    val recvPipe = (in: Stream[IO, WebSocketFrame]) => in.as(())
    runServer(recvPipe) { case (server, serverUri) =>
      val client = WSClientImpl[IO]()

      client.connectHighLevel(WSRequest(serverUri)).use { connection =>
        IO.delay {
          connectionEstablished = true
        }
      }
    }

    connectionEstablished shouldBe true
  }

  "Client should be able to send a message" in {
    var receivedText = false
    runServer
      { (recvStream: Stream[IO, WebSocketFrame]) =>
        recvStream.evalMap {
          case WebSocketFrame.Text(data, last) if data == "foo" =>
            IO.delay { receivedText = true }
        }
      }
      { case (server, serverUri) =>
      val client = WSClientImpl[IO]()

      client.connectHighLevel(WSRequest(serverUri)).use { connection =>
        connection.send(Text("foo"))
      }
    }

    receivedText shouldBe true
  }
}