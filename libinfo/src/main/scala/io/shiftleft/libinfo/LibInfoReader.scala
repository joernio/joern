package io.shiftleft.libinfo

import com.amazon.ion.IonType
import com.amazon.ion.system.IonReaderBuilder

import java.io.{Closeable, InputStream}

class LibInfoReader(inStream: InputStream) extends Iterator[LibInfoElement], Closeable {
  private val reader          = IonReaderBuilder.standard().build(inStream)
  private var nextConsumed    = true
  private var hasNextInternal = false

  override def hasNext: Boolean = {
    if (nextConsumed) {
      nextConsumed = false
      hasNextInternal = reader.next() != null
      hasNextInternal
    } else {
      hasNextInternal
    }

  }

  override def next(): LibInfoElement = {
    nextConsumed = true
    val element = readLibraryInfoElement()
    element
  }

  private def readLibraryInfoElement(): LibInfoElement = {
    stepIntoStruct()

    reader.next()
    val fieldName = reader.getFieldName
    if (fieldName != "label") {
      throw new RuntimeException(s"Label must be first field in a STRUCT but got field name: $fieldName")
    }
    val label = reader.stringValue()

    val newElement =
      label match {
        case "JAVA_CLASS" =>
          readJavaClass()
      }

    stepOutOfStruct()

    newElement
  }

  private def readJavaClass(): JavaClass = {
    var name         = Option.empty[Symbol]
    var signature    = Option.empty[Symbol]
    var access       = Option.empty[BitField]
    var fields       = collection.Seq.empty[JavaField]
    var methods      = collection.Seq.empty[JavaMethod]
    var innerClasses = collection.Seq.empty[JavaInnerClass]

    forNextValues {
      reader.getFieldName match {
        case "name" =>
          name = Some(reader.stringValue())
        case "signature" =>
          signature = Some(reader.stringValue())
        case "access" =>
          access = Some(reader.intValue())
        case "fields" =>
          fields = readJavaFields()
        case "methods" =>
          methods = readJavaMethods()
        case "innerClasses" =>
          innerClasses = readJavaInnerClasses()
      }
    }

    JavaClass(name.get, signature.get, access.get, fields, methods, innerClasses)
  }

  private def readJavaFields(): List[JavaField] = {
    var fields = List.empty[JavaField]

    stepIntoList()

    forNextValues {
      fields = readJavaField() :: fields
    }

    stepOutOfList()

    fields.reverse
  }

  private def readJavaField(): JavaField = {
    var name    = Option.empty[Symbol]
    var typeRef = Option.empty[Symbol]
    var access  = Option.empty[BitField]

    stepIntoStruct()

    forNextValues {
      reader.getFieldName match {
        case "name" =>
          name = Some(reader.stringValue())
        case "typeRef" =>
          typeRef = Some(reader.stringValue())
        case "access" =>
          access = Some(reader.intValue())
      }
    }

    stepOutOfStruct()

    JavaField(name.get, typeRef.get, access.get)
  }

  private def readJavaMethods(): List[JavaMethod] = {
    var methods = List.empty[JavaMethod]

    stepIntoList()

    forNextValues {
      methods = readJavaMethod() :: methods
    }

    stepOutOfList()

    methods.reverse
  }

  private def readJavaMethod(): JavaMethod = {
    var name      = Option.empty[Symbol]
    var signature = Option.empty[Symbol]
    var access    = Option.empty[BitField]

    stepIntoStruct()

    forNextValues {
      reader.getFieldName match {
        case "name" =>
          name = Some(reader.stringValue())
        case "signature" =>
          signature = Some(reader.stringValue())
        case "access" =>
          access = Some(reader.intValue())
      }
    }

    stepOutOfStruct()

    JavaMethod(name.get, signature.get, access.get)
  }

  private def readJavaInnerClasses(): List[JavaInnerClass] = {
    var innerClasses = List.empty[JavaInnerClass]

    stepIntoList()

    forNextValues {
      innerClasses = readJavaInnerClass() :: innerClasses
    }

    stepOutOfList()

    innerClasses.reverse
  }

  private def readJavaInnerClass(): JavaInnerClass = {
    var name   = Option.empty[Symbol]
    var access = Option.empty[BitField]

    stepIntoStruct()

    forNextValues {
      reader.getFieldName match {
        case "name" =>
          name = Some(reader.stringValue())
        case "access" =>
          access = Some(reader.intValue())
      }
    }

    stepOutOfStruct()

    JavaInnerClass(name.get, access.get)
  }

  private def forNextValues(func: => Unit): Unit = {
    var valueType: IonType = null
    while ({
      valueType = reader.next;
      valueType != null
    }) {
      func
    }
  }

  private def stepIntoList(): Unit = {
    if (reader.getType != IonType.LIST) {
      throw new RuntimeException(s"Expected LIST but found ${reader.getType.name()}")
    }

    reader.stepIn()
  }

  private def stepOutOfList(): Unit = {
    reader.stepOut()
  }

  private def stepIntoStruct(): Unit = {
    if (reader.getType != IonType.STRUCT) {
      throw new RuntimeException(s"Expected STRUCT but found ${reader.getType.name()}")
    }

    reader.stepIn()
  }

  private def stepOutOfStruct(): Unit = {
    reader.stepOut()
  }

  override def close(): Unit = {
    inStream.close()
  }
}
