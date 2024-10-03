package io.shiftleft.semanticcpg.typeinfo.loading

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.semanticcpg.typeinfo.loading.BytesLoader
import io.shiftleft.semanticcpg.typeinfo.dependencies._
import io.shiftleft.semanticcpg.typeinfo.version.Version

import scala.annotation.tailrec
import scala.util.Using

class DirectDependencyIonTextLoader(versionParser: String => Version) extends BytesLoader[List[DirectDependency]] {
  private val defaultDependency = DirectDependency("", Any())
  
  override def loadFromBytes(data: Array[Byte]): List[DirectDependency] = {
    Using.resource(IonReaderBuilder.standard().build(data))(parseLoop(_))
  }
  
  @tailrec
  private def parseLoop(reader: IonReader, deps: List[DirectDependency] = Nil): List[DirectDependency] = {
    val ty = Option(reader.next())
    ty match
      case None => deps
      case Some(IonType.STRUCT) => {
        reader.stepIn()
        val dependency = parseDependencyStruct(reader, defaultDependency)
        reader.stepOut()
        parseLoop(reader, dependency :: deps)
      }
      case Some(IonType.LIST) => {
        reader.stepIn()
        parseLoop(reader, deps)
      }
  }

  @tailrec
  private def parseDependencyStruct(r: IonReader, dep: DirectDependency): DirectDependency = {
    Option(r.next()) match
      case None => dep
      case Some(_) =>
        r.getFieldName match
          case "NAME"       => parseDependencyStruct(r, dep.copy(name = r.stringValue()))
          case "CONSTRAINT" => parseDependencyStruct(r, dep.copy(version = VersionConstraint.parse(r.stringValue(), versionParser)))
  }
}
