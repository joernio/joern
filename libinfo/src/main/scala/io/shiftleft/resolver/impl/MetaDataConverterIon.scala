package io.shiftleft.resolver.impl

import com.amazon.ion.{IonReader, IonType, IonWriter}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.resolver.api.{Coordinate, Id, IdConverterIon, MetaData}
import io.shiftleft.resolver.util.IonUtil.forNextValues

class MetaDataConverterIon[I <: Id](coordinateConverter: CoordinateConverterIon[I]) {

  def readMetaData(reader: IonReader): MetaData[I] = {
    var coordinate = Option.empty[Coordinate[I]]
    var dependencies = Vector.empty[Coordinate[I]]

    reader.next()
    reader.stepIn()

    forNextValues(reader, {
      reader.getFieldName match {
        case "coordinate" =>
          coordinate = Some(coordinateConverter.readCoordinate(reader))
        case "dependencies" =>
          dependencies = readDependencies(reader)
      }
    })

    reader.stepOut()
    
    MetaData(coordinate.get, dependencies)
  }

  private def readDependencies(reader: IonReader): Vector[Coordinate[I]] = {
    var dependencies = Vector.empty[Coordinate[I]]
    
    reader.stepIn()

    forNextValues(reader, {
      dependencies = dependencies.appended(coordinateConverter.readCoordinate(reader))
    })

    reader.stepOut()
    
    dependencies
  }
  
  def writeMetaData(writer: IonWriter, metaData: MetaData[I]): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("coordinate")
    coordinateConverter.writeCoordinate(writer, metaData.coordinate)

    if (metaData.directDeps.nonEmpty) {
      writer.setFieldName("dependencies")
      writer.stepIn(IonType.LIST)

      metaData.directDeps.foreach { dep =>
        coordinateConverter.writeCoordinate(writer, dep)
      }

      writer.stepOut()
    }

    writer.stepOut()
  }
}
