package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonType, IonWriter}
import com.amazon.ion.system.IonTextWriterBuilder

import java.io.ByteArrayOutputStream

object IonWriter extends Writer  {
  def writeToString(ty: TypeDecl): String = {
    val out = ByteArrayOutputStream()
    val w: IonWriter = IonTextWriterBuilder.pretty().build(out)

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

    w.setFieldName("DEPENDS")
    w.stepIn(IonType.LIST)
    ty.dependencies.foreach(writeDependency(w))
    w.stepOut() // dependency list

    w.stepOut() // main struct

    out.toString
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

  private def writeDependency(w: IonWriter)(d: Dependency): Unit = {
    w.stepIn(IonType.STRUCT)
    w.setFieldName("FULL_NAME")
    w.writeString(d.fullName)
    d.version match
      case Some(v) => {
        w.setFieldName("VERSION")
        w.writeString(v)
      }
      case _ => // do nothing
    w.stepOut()
  }
}