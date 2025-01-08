package io.shiftleft.libinfo

import com.amazon.ion.IonType
import com.amazon.ion.system.IonTextWriterBuilder

import java.io.{Closeable, OutputStream}

class LibInfoWriter(outStream: OutputStream) extends Closeable {
  private val writer = IonTextWriterBuilder.pretty().build(outStream)

  override def close(): Unit = {
    writer.close()
  }

  def writeLibInfoElement(element: LibInfoElement): Unit = {
    element match {
      case javaClass: JavaClass =>
        writeJavaClass(javaClass)
    }
  }

  def writeJavaClass(javaClass: JavaClass): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("label")
    writer.writeSymbol("JAVA_CLASS")

    writer.setFieldName("name")
    writer.writeSymbol(javaClass.name)

    writer.setFieldName("signature")
    writer.writeSymbol(javaClass.signature)

    writer.setFieldName("access")
    writer.writeInt(javaClass.access)

    if (javaClass.fields.nonEmpty) {
      writer.setFieldName("fields")
      writer.stepIn(IonType.LIST)
      javaClass.fields.foreach(writeJavaField)
      writer.stepOut()
    }

    if (javaClass.methods.nonEmpty) {
      writer.setFieldName("methods")
      writer.stepIn(IonType.LIST)
      javaClass.methods.foreach(writeJavaMethod)
      writer.stepOut()
    }

    if (javaClass.innerClasses.nonEmpty) {
      writer.setFieldName("innerClasses")
      writer.stepIn(IonType.LIST)
      javaClass.innerClasses.foreach(writeJavaInnerClassInfo)
      writer.stepOut()
    }

    writer.stepOut()
  }

  private def writeJavaField(javaField: JavaField): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("name")
    writer.writeSymbol(javaField.name)

    writer.setFieldName("typeRef")
    writer.writeSymbol(javaField.typeRef)

    writer.setFieldName("access")
    writer.writeInt(javaField.access)

    writer.stepOut()
  }

  private def writeJavaMethod(javaMethod: JavaMethod): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("name")
    writer.writeSymbol(javaMethod.name)

    writer.setFieldName("signature")
    writer.writeSymbol(javaMethod.signature)

    writer.setFieldName("access")
    writer.writeInt(javaMethod.access)

    writer.stepOut()
  }

  private def writeJavaInnerClassInfo(javaInnerClassInfo: JavaInnerClass): Unit = {
    writer.stepIn(IonType.STRUCT)

    writer.setFieldName("name")
    writer.writeSymbol(javaInnerClassInfo.name)

    writer.setFieldName("access")
    writer.writeInt(javaInnerClassInfo.access)

    writer.stepOut()
  }
}
