package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.Writer
import org.json4s.*
import org.json4s.FieldSerializer.*
import org.json4s.native.JsonMethods.*
import org.json4s.native.{Serialization, prettyJson, renderJValue}

import java.util.zip.ZipOutputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.Charset
import scala.util.{Try, Using}


object JsonWriter extends Writer {
  implicit val format: Formats = JsonLoader.format // TODO

  def writeToString(ty: TypeDecl): Try[String] = Try(Serialization.writePretty(ty))
  
  def writeToBinaryFormat(ty: TypeDecl): Try[Array[Byte]] = 
    Using.Manager { use =>
      val bytes = use(ByteArrayOutputStream())
      val zip = use(ZipOutputStream(bytes))
      zip.write(Serialization.writePretty(ty).getBytes("UTF-8"))
      bytes.toByteArray
    }
}
