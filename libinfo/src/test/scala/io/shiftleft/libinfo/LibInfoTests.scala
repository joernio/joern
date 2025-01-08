package io.shiftleft.libinfo

import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.util.Using

class LibInfoTests extends AnyWordSpec {
  "test plain java class without fields, methods or inner classes" in {
    test(
      JavaClass(
        name = "some/package/MyInterface",
        signature = "<AAA:Ljava/lang/Object;>Ljava/lang/Object;",
        access = JavaAccessBits.JavaPublic | JavaAccessBits.JavaInterface,
        fields = Nil,
        methods = Nil,
        innerClasses = Nil
      )
    )
  }

  "test java class with fields, methods and inner classes" in {
    val fields = List(
      JavaField(name = "field1", typeRef = "Lsome/package/Class1;", access = JavaAccessBits.JavaPublic),
      JavaField(name = "field2", typeRef = "Lsome/package/Class2;", access = JavaAccessBits.JavaPublic)
    )

    val methods = List(
      JavaMethod(name = "method1", signature = "()Lsome/package/Class1", access = JavaAccessBits.JavaPublic),
      JavaMethod(name = "method2", signature = "()Lsome/package/Class2", access = JavaAccessBits.JavaPublic)
    )

    val innerClasses = List(
      JavaInnerClass(name = "SomeInnerClass1", access = JavaAccessBits.JavaPublic),
      JavaInnerClass(name = "SomeInnerClass2", access = JavaAccessBits.JavaPublic)
    )

    test(
      JavaClass(
        name = "some/package/MyInterface",
        signature = "Ljava/lang/Object;",
        access = JavaAccessBits.JavaPublic | JavaAccessBits.JavaInterface,
        fields = fields,
        methods = methods,
        innerClasses = innerClasses
      )
    )
  }

  def test(libInfoElement: LibInfoElement): Unit = {
    test(libInfoElement :: Nil)
  }

  def test(inputLibInfoElements: List[LibInfoElement]): Unit = {
    val outStream = new ByteArrayOutputStream()
    Using.resource(LibInfoWriter(outStream)) { writer =>
      inputLibInfoElements.foreach(writer.writeLibInfoElement)
    }

    val contentByteArray = outStream.toByteArray
    val inStream         = new ByteArrayInputStream(contentByteArray)
    val readLibInfoElements = Using
      .resource(LibInfoReader(inStream)) { reader =>
        reader
      }
      .toList

    readLibInfoElements should contain theSameElementsInOrderAs inputLibInfoElements
  }
}
