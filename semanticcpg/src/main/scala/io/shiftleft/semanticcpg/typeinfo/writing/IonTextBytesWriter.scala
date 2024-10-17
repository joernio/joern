package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonType, IonWriter}
import com.amazon.ion.system.{IonBinaryWriterBuilder, IonTextWriterBuilder}

import java.io.{ByteArrayOutputStream, OutputStream}
import scala.util.{Failure, Success, Try, Using}

object IonTextBytesWriter extends BytesWriter[TypeDecl] {
  def write(ty: TypeDecl): Array[Byte] = {
    val writeResult = Using.Manager { use =>
      val out = use(ByteArrayOutputStream())
      val w   = use(IonBinaryWriterBuilder.standard().build(out))
      writeType(ty, w)
      w.finish()
      out.toByteArray
    }
    writeResult match {
      case Success(bytes: Array[Byte]) => bytes
      case Failure(exc: Throwable)     => throw exc
    }
  }

  private def writeType(ty: TypeDecl, writer: IonWriter): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("FULL_NAME")
    writer.writeString(ty.fullName)

    writer.setFieldName("NAME")
    writer.writeString(ty.name)

    writer.setFieldName("TYPE_PARAMETERS")
    writer.stepIn(IonType.LIST)
    ty.typeParams.foreach(writer.writeString)
    writer.stepOut()

    writer.setFieldName("INHERITS")
    writer.stepIn(IonType.LIST)
    ty.inherits.foreach(writer.writeString)
    writer.stepOut()

    writer.setFieldName("METHODS")
    writer.stepIn(IonType.LIST)
    ty.methods.foreach(writeMethod(writer))
    writer.stepOut()

    writer.setFieldName("MEMBERS")
    writer.stepIn(IonType.LIST)
    ty.members.foreach(writeMember(writer))
    writer.stepOut()

    writer.stepOut() // main struct
  }

  private def writeMethod(writer: IonWriter)(method: Method): Unit = {
    writer.stepIn(IonType.STRUCT)
    writer.setFieldName("NAME")
    writer.writeString(method.name)
    writer.setFieldName("FULL_NAME")
    writer.writeString(method.fullName)
    writer.setFieldName("SIGNATURE")
    writer.writeString(method.signature)
    writer.stepOut()
  }

  private def writeMember(writer: IonWriter)(member: Member): Unit = {
    writer.stepIn(IonType.STRUCT)
    writer.setFieldName("NAME")
    writer.writeString(member.name)
    writer.setFieldName("TYPE_FULL_NAME")
    writer.writeString(member.typeFullName)
    writer.stepOut()
  }
}
