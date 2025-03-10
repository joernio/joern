package io.shiftleft.resolver.impl

import com.amazon.ion.{IonReader, IonType, IonWriter}
import io.shiftleft.resolver.api.IdConverterIon
import io.shiftleft.resolver.util.IonUtil.forNextValues

class IdConverterIonMaven extends IdConverterIon[IdMaven] {
  override def readId(reader: IonReader): IdMaven = {
    reader.stepIn()

    var groupId = Option.empty[String]
    var artifactId = Option.empty[String]

    forNextValues(reader, {
      reader.getFieldName match {
        case "kind" =>
          val kind = reader.stringValue()
          if (kind != "mvn") {
            throw new RuntimeException(s"Unexpected id kind $kind")
          }
        case "groupId" =>
          groupId = Some(reader.stringValue())
        case "artifactId" =>
          artifactId = Some(reader.stringValue())
      }
    })

    reader.stepOut()

    IdMaven(groupId.get, artifactId.get)
  }

  override def writeId(writer: IonWriter, id: IdMaven): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("kind")
    writer.writeString("mvn")

    writer.setFieldName("groupId")
    writer.writeString(id.groupId)

    writer.setFieldName("artifactId")
    writer.writeString(id.artifactId)

    writer.stepOut()
  }
}
