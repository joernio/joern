package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonType, IonWriter}
import com.amazon.ion.system.{IonBinaryWriterBuilder, IonTextWriterBuilder}

import java.io.{ByteArrayOutputStream, OutputStream}
import scala.util.{Try, Using}

object IonWriter extends Writer[TypeDecl] {
  override def writeToString(ty: TypeDecl): Try[String] = 
    Using.Manager { use =>
      val out = use(ByteArrayOutputStream())
      val w = use(IonTextWriterBuilder.pretty().build(out))
      writeType(ty, w)
      w.finish()
      out.toString
    }

  def writeToBinaryFormat(ty: TypeDecl): Try[Array[Byte]] =
    Using.Manager { use => 
      val out = use(ByteArrayOutputStream())
      val w = use(IonBinaryWriterBuilder.standard().build(out))
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
  
  private def writeType(ty: TypeDecl, w: IonWriter): Unit = {
    w.stepIn(IonType.STRUCT)

    w.setFieldName("FULL_NAME")
    w.writeString(ty.fullName)

    w.setFieldName("NAME")
    w.writeString(ty.name)

    w.setFieldName("TYPE_PARAMETERS")
    w.stepIn(IonType.LIST)
    ty.typeParams.foreach(w.writeString)
    w.stepOut()

    w.setFieldName("INHERITS")
    w.stepIn(IonType.LIST)
    ty.inherits.foreach(w.writeString)
    w.stepOut()

    w.setFieldName("METHODS")
    w.stepIn(IonType.LIST)
    ty.methods.foreach(writeMethod(w))
    w.stepOut()

    w.setFieldName("MEMBERS")
    w.stepIn(IonType.LIST)
    ty.members.foreach(writeMember(w))
    w.stepOut()

    w.stepOut() // main struct
  }
  
  private def writeMethod(w: IonWriter)(m: Method): Unit = {
    w.stepIn(IonType.STRUCT)
    w.setFieldName("NAME")
    w.writeString(m.name)
    w.setFieldName("FULL_NAME")
    w.writeString(m.fullName)
    w.setFieldName("SIGNATURE")
    w.writeString(m.signature)
    w.stepOut()
  }
  
  private def writeMember(w: IonWriter)(m: Member): Unit = {
    w.stepIn(IonType.STRUCT)
    w.setFieldName("NAME")
    w.writeString(m.name)
    w.setFieldName("TYPE_FULL_NAME")
    w.writeString(m.typeFullName)
    w.stepOut()
  }
}
