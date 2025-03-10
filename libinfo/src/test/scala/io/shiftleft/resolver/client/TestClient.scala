package io.shiftleft.resolver.client

import cats.effect.{ExitCode, IO, IOApp}
import io.shiftleft.resolver.client.WSClientImpl
import io.shiftleft.resolver.impl.{CoordinateConverterIon, IdConverterIonMaven, MetaDataCalculatorRemote}
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.Uri
import org.http4s.client.websocket.WSRequest
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
