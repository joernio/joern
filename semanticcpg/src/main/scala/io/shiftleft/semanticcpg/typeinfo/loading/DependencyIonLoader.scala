package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.dependencies.*
import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.Java

import java.io.{IOException, InputStream}
import scala.annotation.tailrec
import scala.util.{Failure, Try, Using};

object DependencyIonLoader {

  /** TODO: load resolved transitive dependencies vs load direct dependencies */
  def parse(lang: LanguagePlatform, data: String): Try[List[DirectDependency]] = {
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      parseLoop(reader, lang)
    }
  }

  def parse(lang: LanguagePlatform, data: InputStream): Try[List[DirectDependency]] = {
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      parseLoop(reader, lang)
    }
  }

  private def parseLoop(r: IonReader, lang: LanguagePlatform, deps: List[DirectDependency] = Nil): List[DirectDependency] = {
    val ty = Option(r.next())
    ty match
      case None => deps
      case Some(IonType.STRUCT) => {
        r.stepIn()
        val dependency = parseDependencyStruct(r, defaultDependency(lang))
        r.stepOut()
        parseLoop(r, lang, dependency :: deps)
      }
      case Some(IonType.LIST) => {
        r.stepIn()
        parseLoop(r, lang, deps)
      }
  }

  private def defaultDependency(lang: LanguagePlatform): DirectDependency = {
    DirectDependency(PackageIdentifier(lang, ""), Any())
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
