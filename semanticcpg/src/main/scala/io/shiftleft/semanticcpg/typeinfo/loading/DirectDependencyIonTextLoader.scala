package io.shiftleft.semanticcpg.typeinfo.loading

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.semanticcpg.typeinfo.dependencies.*
import io.shiftleft.semanticcpg.typeinfo.version.Version

import java.io.InputStream
import scala.annotation.tailrec
import scala.util.Using

object DirectDependencyIonTextLoader {
  private val defaultDependency = DirectDependency("", Any())

  def loadFromStream(versionParser: String => Version, data: InputStream): List[DirectDependency] = {
    Using.resource(IonReaderBuilder.standard().build(data))(parseLoop(_, versionParser))
  }

  @tailrec
  private def parseLoop(
    reader: IonReader,
    versionParser: String => Version,
    deps: List[DirectDependency] = Nil
  ): List[DirectDependency] = {
    val ty = Option(reader.next())
    ty match
      case None => deps
      case Some(IonType.STRUCT) => {
        reader.stepIn()
        val dependency = parseDependencyStruct(reader, versionParser, defaultDependency)
        reader.stepOut()
        parseLoop(reader, versionParser, dependency :: deps)
      }
      case Some(IonType.LIST) => {
        reader.stepIn()
        parseLoop(reader, versionParser, deps)
      }
  }

  @tailrec
  private def parseDependencyStruct(
                                     reader: IonReader,
                                     versionParser: String => Version,
                                     dep: DirectDependency
  ): DirectDependency = {
    Option(reader.next()) match
      case None => dep
      case Some(_) =>
        reader.getFieldName match
          case "NAME" => parseDependencyStruct(reader, versionParser, dep.copy(name = reader.stringValue()))
          case "VERSION_CONSTRAINT" =>
            parseDependencyStruct(
              reader,
              versionParser,
              dep.copy(version = VersionConstraint.parse(reader.stringValue(), versionParser))
            )
  }
}
