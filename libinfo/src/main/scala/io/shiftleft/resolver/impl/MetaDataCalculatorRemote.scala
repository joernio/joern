package io.shiftleft.resolver.impl

import cats.effect.{IO, Sync}
import cats.syntax.all.*
import com.amazon.ion.IonType
import com.amazon.ion.system.IonTextWriterBuilder
import fs2.Stream
import io.shiftleft.resolver.api.{Coordinate, Id, MetaDataCalculator}
import org.http4s.client.websocket.{WSConnection, WSConnectionHighLevel}
import org.http4s.client.websocket.WSFrame.Text
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.ByteArrayOutputStream


class MetaDataCalculatorRemote[F[_]: Sync, I <: Id](connection: WSConnectionHighLevel[F],
                                                    coordinateConverter: CoordinateConverterIon[I]) extends MetaDataCalculator[F, I] {
  private given Logger[F] = Slf4jLogger.getLogger[F]()

  override def calculateMetaData(deps: Vector[Coordinate[I]]): F[Unit] = {
    val message = createMessage(deps)
    connection.send(Text(message)).flatMap { _ =>
      Logger[F].info(s"Send message") >>
        connection.receive.flatMap { responseFrame =>
          Logger[F].info(s"Remote calculation finished with $responseFrame")
        }
    }
  }

  private def createMessage(deps: Vector[Coordinate[I]]): String = {
    val outStream = new ByteArrayOutputStream()
    val writer = IonTextWriterBuilder.standard().build(outStream)
    writer.stepIn(IonType.LIST)
    deps.foreach(coordinateConverter.writeCoordinate(writer, _))
    writer.stepOut()
    writer.close()
    outStream.toString
  }
}
