package io.shiftleft.http4s

import cats.ApplicativeError
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.comcast.ip4s.{Port, ipv4, port}
import fs2.io.net.BindException
import io.shiftleft.http4s.WSClientImpl
import io.shiftleft.resolver.impl.{CoordinateConverterIon, IdConverterIonMaven, MetaDataCalculatorRemote}
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.{HttpApp, Uri}
import org.http4s.client.websocket.WSRequest
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.server.websocket.WebSocketBuilder2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.syntax.all.*

class WSClientImplTests extends AnyWordSpec with Matchers {
  def createWSServer(handler: WebSocketBuilder2[IO] => HttpApp[IO], port: Int = 1023)
                    (implicit F: ApplicativeError[IO, Throwable]): Resource[IO, Server] = {
    val server = EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(Port.fromInt(port).get)
      .withHttpWebSocketApp(handler)
      .build

    server.handleErrorWith { (error: Throwable) =>
      createWSServer(handler, port + 1)
    }
  }

  "Client should be able to connect" in {
    new IOApp {
      override def reportFailure(err: Throwable): IO[Unit] = {
        err match {
          case _: BindException =>
            IO.unit
          case _ =>
            IO.blocking(err.printStackTrace())
        }
      }
      override def run(args: List[String]): IO[ExitCode] = {

        createWSServer(_ => Router.apply[IO]().orNotFound).use { server =>
          IO.println(server.address.getPort) >>
            IO.trace.flatMap(trace => IO.println(trace.pretty)) >>
            IO.unit
        }.as(ExitCode.Success)

      }
    }.main(Array.empty)
  }
}

object TestClient extends IOApp {
  private given Logger[IO] = Slf4jLogger.getLogger[IO]()

  Configurator.setRootLevel(Level.TRACE)

  override def run(args: List[String]): IO[ExitCode] = {
    val client = WSClientImpl[IO]()
    val uri = Uri.fromString("ws://localhost:8080/test").toOption.get

    client.connectHighLevel(WSRequest(uri))
      .use { connection =>
        val idConverter = IdConverterIonMaven()
        val coordinateConverter = CoordinateConverterIon(idConverter)
        val calculator = MetaDataCalculatorRemote(connection, coordinateConverter)

        calculator.calculateMetaData(Vector.empty)
      }
      .as(ExitCode.Success)

  }
}
