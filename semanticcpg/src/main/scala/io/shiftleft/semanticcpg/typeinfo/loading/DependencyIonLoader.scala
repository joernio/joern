package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.dependencies.*
import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM

import java.io.{IOException, InputStream}
import scala.annotation.tailrec
import scala.util.{Failure, Try, Using};

object DependencyIonLoader {

  /** TODO: load resolved transitive dependencies vs load direct dependencies */
  def parse(platform: LanguagePlatform, data: String): Try[List[DirectDependency]] = {
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      parseLoop(reader, platform)
    }
  }

  def parse(platform: LanguagePlatform, data: InputStream): Try[List[DirectDependency]] = {
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      parseLoop(reader, platform)
    }
  }

  private def parseLoop(reader: IonReader, platform: LanguagePlatform, deps: List[DirectDependency] = Nil): List[DirectDependency] = {
    val ty = Option(reader.next())
    ty match
      case None => deps
      case Some(IonType.STRUCT) => {
        reader.stepIn()
        val dependency = parseDependencyStruct(reader, defaultDependency(platform))
        reader.stepOut()
        parseLoop(reader, platform, dependency :: deps)
      }
      case Some(IonType.LIST) => {
        reader.stepIn()
        parseLoop(reader, platform, deps)
      }
  }

  private def defaultDependency(platform: LanguagePlatform): DirectDependency = {
    DirectDependency(PackageIdentifier(platform, ""), Any())
  }

  private def parseDependencyStruct(r: IonReader, dep: DirectDependency): DirectDependency = {
    Option(r.next()) match
      case None => dep
      case Some(_) =>
        r.getFieldName() match
          case "NAME"       => parseDependencyStruct(r, dep.copy(name = dep.name.copy(name = r.stringValue())))
          case "CONSTRAINT" => parseDependencyStruct(r, dep.copy(version = VersionConstraint.parse(r.stringValue())))
          case "LANG" =>
            parseDependencyStruct(r, dep.copy(name = dep.name.copy(platform = LanguagePlatform.ofString(r.stringValue()))))
  }
}
