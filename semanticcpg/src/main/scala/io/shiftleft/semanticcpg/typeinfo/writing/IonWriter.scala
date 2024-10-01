package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonType, IonWriter}
import com.amazon.ion.system.{IonBinaryWriterBuilder, IonTextWriterBuilder}

import java.io.{ByteArrayOutputStream, OutputStream}
import scala.util.{Try, Using}

object IonWriter extends Writer[TypeDecl] {
  override def writeToString(ty: TypeDecl): Try[String] =
    Using.Manager { use =>
      val out = use(ByteArrayOutputStream())
      val w   = use(IonTextWriterBuilder.pretty().build(out))
      writeType(ty, w)
      w.finish()
      out.toString
    }

  def writeToBinaryFormat(ty: TypeDecl): Try[Array[Byte]] =
    Using.Manager { use =>
      val out = use(ByteArrayOutputStream())
      val w   = use(IonBinaryWriterBuilder.standard().build(out))
      writeType(ty, w)
      w.finish()
      out.toByteArray
    }

  override def writeToStream(ty: TypeDecl, os: OutputStream): Try[Unit] = {
    Using.Manager { use =>
      val w = use(IonTextWriterBuilder.pretty().build(os))
      writeType(ty, w)
      w.finish()
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
