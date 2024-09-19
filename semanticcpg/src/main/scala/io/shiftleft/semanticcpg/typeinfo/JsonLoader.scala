package io.shiftleft.semanticcpg.typeinfo

import org.json4s.*
import org.json4s.FieldSerializer.*
import org.json4s.native.JsonMethods.*
import org.json4s.native.Serialization

import java.io.ByteArrayInputStream
import java.util.zip.ZipInputStream
import scala.util.{Try, Using};

object JsonLoader extends Loader {
  private val dependencySerializer = FieldSerializer[Dependency](
    renameTo("fullName", "FULL_NAME") orElse renameTo("version", "VERSION"),
    renameFrom("FULL_NAME", "fullName") orElse renameFrom("VERSION", "version"),
  )

  private val memberSerializer = FieldSerializer[Member](
    renameTo("typeFullName", "TYPE_FULL_NAME") orElse renameTo("name", "NAME"),
    renameFrom("TYPE_FULL_NAME", "typeFullName") orElse renameFrom("NAME", "name")
  )

  private val methodSerializer = FieldSerializer[Method](
    renameTo("fullName", "FULL_NAME") orElse renameTo("name", "NAME") orElse renameTo("signature", "SIGNATURE"),
    renameFrom("FULL_NAME", "fullName") orElse renameFrom("NAME", "name") orElse renameFrom("SIGNATURE", "signature")
  )

  private val typeDeclSerializer = FieldSerializer[TypeDecl](
    renameTo("fullName", "FULL_NAME") orElse 
      renameTo("name", "NAME") orElse 
      renameTo("typeParams", "TYPE_PARAMETERS") orElse
      renameTo("inherits", "INHERITS") orElse
      renameTo("methods", "METHODS") orElse
      renameTo("members", "MEMBERS") orElse
      renameTo("dependencies", "DEPENDS"),
    renameFrom("FULL_NAME", "fullName") orElse 
      renameFrom("NAME", "name") orElse
      renameFrom("TYPE_PARAMETERS", "typeParams") orElse
      renameFrom("INHERITS", "inherits") orElse
      renameFrom("METHODS", "methods") orElse
      renameFrom("MEMBERS", "members") orElse
      renameFrom("DEPENDS", "dependencies")  
  )

  // TODO
  implicit val format: Formats = DefaultFormats + dependencySerializer + memberSerializer + methodSerializer + typeDeclSerializer

  override def parse(data: String): Try[TypeDecl] = Try(Serialization.read(data))
  
  override def parse(data: Array[Byte]): Try[TypeDecl] = 
    Using.Manager { use =>
      val bytes = use(ByteArrayInputStream(data))
      val zip = use(ZipInputStream(bytes))
      val jsonStr = String(zip.readAllBytes(), "UTF-8")
      Serialization.read(jsonStr)
    }
}
