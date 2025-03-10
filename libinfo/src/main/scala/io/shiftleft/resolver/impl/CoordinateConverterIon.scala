package io.shiftleft.resolver.impl

import com.amazon.ion.{IonReader, IonType, IonWriter}
import io.shiftleft.resolver.api.{Coordinate, Id, IdConverterIon}
import io.shiftleft.resolver.util.IonUtil.forNextValues

class CoordinateConverterIon[I <: Id](idConverter: IdConverterIon[I]) {
  def readCoordinate(reader: IonReader): Coordinate[I] = {
    var id = Option.empty[I]
    var version = Option.empty[String]

    reader.stepIn()

    forNextValues(reader, {
      reader.getFieldName match {
        case "id" =>
          id = Some(idConverter.readId(reader))
        case "version" =>
          version = Some(reader.stringValue())
      }
    })

    reader.stepOut()

    Coordinate(id.get, version.get)
  }

  def writeCoordinate(writer: IonWriter, coordinate: Coordinate[I]): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("id")
    idConverter.writeId(writer, coordinate.id)

    writer.setFieldName("version")
    writer.writeString(coordinate.version)

    writer.stepOut()
  }
}

