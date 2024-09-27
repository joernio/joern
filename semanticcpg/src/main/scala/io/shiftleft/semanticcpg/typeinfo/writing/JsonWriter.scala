package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.Writer
import org.json4s.*
import org.json4s.FieldSerializer.*
import org.json4s.native.JsonMethods.*
import org.json4s.native.{Serialization, prettyJson, renderJValue}

import java.util.zip.{ZipEntry, ZipOutputStream}
import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.Charset
import scala.util.{Try, Using}

object JsonWriter extends Writer[TypeDecl] {
  private val defaultZipEntry  = ZipEntry("file")
  implicit val format: Formats = JsonLoader.format // TODO

  def writeToString(ty: TypeDecl): Try[String] = Try(Serialization.writePretty(ty))

  def writeToBinaryFormat(ty: TypeDecl): Try[Array[Byte]] =
    Using.Manager { use =>
      val bytes = use(ByteArrayOutputStream())
      val zip   = use(ZipOutputStream(bytes))
      zip.putNextEntry(defaultZipEntry)
      zip.write(Serialization.writePretty(ty).getBytes("UTF-8"))
      zip.closeEntry()
      bytes.toByteArray
    }

  override def writeToStream(ty: TypeDecl, os: OutputStream): Try[Unit] =
    for {
      prettyString <- Try(Serialization.writePretty(ty))
      _            <- Try(os.write(prettyString.getBytes("UTF-8")))
    } yield ()
}
