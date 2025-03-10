package io.shiftleft.resolver.server

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp}
import com.amazon.ion.system.IonReaderBuilder
import com.comcast.ip4s.*
import fs2.Stream
import io.shiftleft.resolver.api.Coordinate
import io.shiftleft.resolver.impl.{CoordinateConverterIon, IdConverterIonMaven, IdMaven, MetaDataCalculatorLocal, MetaDataConverterIon, MetaDataFetcherMaven, MetaDataStoreGit}
import io.shiftleft.resolver.server.WebsocketServer.coordinateConverter
import io.shiftleft.resolver.util.IonUtil.forNextValues
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.client.middleware.FollowRedirect

import java.nio.file.Path
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.{HttpApp, HttpRoutes, dsl}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.DurationInt

object WebsocketServer extends IOApp {
  val idConverter = IdConverterIonMaven()
  val coordinateConverter = CoordinateConverterIon(idConverter)
  val metaDataConverter = MetaDataConverterIon(coordinateConverter)
  val serverUrl = "https://repo1.maven.org/maven2"

  Configurator.setRootLevel(Level.TRACE)

  override def run(args: List[String]): IO[ExitCode] = {
    val httpClientR = EmberClientBuilder.default[IO].build.map(FollowRedirect(10))
    httpClientR.use { httpClient =>
      val calculator = new MetaDataCalculatorLocal(MetaDataFetcherMaven[IO](httpClient, serverUrl),
        MetaDataStoreGit(Path.of("/tmp/serverLibInfo"), metaDataConverter))

      new WebsocketServer(calculator).run()
    }
  }

}

class WebsocketServer(calculator: MetaDataCalculatorLocal[IO, IdMaven]) {
  private given Logger[IO] = Slf4jLogger.getLogger[IO]()

  def run(): IO[ExitCode] = {
    val server = EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpWebSocketApp(createHttpApp)
      .build

    server.useForever.as(ExitCode.Success)
  }

  private def createHttpApp(socketBuilder: WebSocketBuilder2[IO]): HttpApp[IO] = {
    val service =
      HttpRoutes.of[IO] {
        case GET -> Root / "test" =>
          Queue.unbounded[IO, Option[WebSocketFrame]].flatMap { queue =>
            val returnMsgStream = Stream.fromQueueNoneTerminated(queue)
            val send = Stream.awakeEvery[IO](10.seconds).map(_ => WebSocketFrame.Ping()).merge(returnMsgStream)
            //val send = Stream.awakeEvery[IO](70.seconds).map(_ => WebSocketFrame.Text("foo\n"))
            val recv = (in: Stream[IO, WebSocketFrame]) => in.evalMap(frame => handleIncomingFrame(frame, queue))
            socketBuilder
              .build(send, recv)
          }
      }
    Router("/" -> service).orNotFound
  }

  private def handleIncomingFrame(frame: WebSocketFrame,
                                  queue: Queue[IO, Option[WebSocketFrame]]): IO[Unit] = {
    frame match {
      case WebSocketFrame.Text((data, last)) =>
        for {
          _ <- Logger[IO].info(s"Received data $data")
          deps = parseDepCoordinates(data)
          _ <- Logger[IO].info(s"Start calculate for $deps")
          _ <- calculator.calculateMetaData(deps)
          _ <- queue.offer(Some(WebSocketFrame.Text("end")))
          _ <- Logger[IO].info(s"Done")
        } yield ()

    }
  }

  private def parseDepCoordinates(data: String): Vector[Coordinate[IdMaven]] = {
    val reader = IonReaderBuilder.standard().build(data)
    var dependencies = Vector.empty[Coordinate[IdMaven]]

    reader.next()
    reader.stepIn()

    forNextValues(reader, {
      dependencies = dependencies.appended(coordinateConverter.readCoordinate(reader))
    })

    reader.stepOut()

    dependencies
  }
}
