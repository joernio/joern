package io.joern.csharpsrc2cpg.utils

import io.joern.semanticcpg.utils.SecureXmlParsing

import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths
import scala.xml.{Elem, Node}

/** Depending on the project type defined in `.csproj` files, different sets of global usings are turned on by default.
  * Here we collect them all.
  */
object ImplicitUsingsCollector {

  /** Collects implicit global imports extracted from `.csproj` files.
    *
    * @param buildFiles
    *   paths to `.csproj` files
    * @return
    *   the list of implicitly turned on global imports
    */
  def collect(buildFiles: List[String]): List[String] = {
    buildFiles.flatMap { csproj =>
      SecureXmlParsing.parseXml(Paths.get(csproj).fileContent) match
        case Some(xml) => from(xml)
        case None      => List.empty
    }
  }

  // See https://learn.microsoft.com/en-gb/dotnet/core/project-sdk/overview#implicit-using-directives
  private val projectTypeMapping: Map[String, List[String]] = {
    val netSdkNamespace = List(
      "System",
      "System.Collections.Generic",
      "System.IO",
      "System.Linq",
      "System.Net.Http",
      "System.Threading",
      "System.Threading.Tasks"
    )
    Map(
      "Microsoft.NET.Sdk" -> netSdkNamespace,
      "Microsoft.NET.Sdk.Web" -> netSdkNamespace.appendedAll(
        List(
          "System.Net.Http.Json",
          "Microsoft.AspNetCore.Builder",
          "Microsoft.AspNetCore.Hosting",
          "Microsoft.AspNetCore.Http",
          "Microsoft.AspNetCore.Routing",
          "Microsoft.Extensions.Configuration",
          "Microsoft.Extensions.DependencyInjection",
          "Microsoft.Extensions.Hosting",
          "Microsoft.Extensions.Logging"
        )
      ),
      "Microsoft.NET.Sdk.Worker" -> netSdkNamespace.appendedAll(
        List(
          "Microsoft.Extensions.Configuration",
          "Microsoft.Extensions.DependencyInjection",
          "Microsoft.Extensions.Hosting",
          "Microsoft.Extensions.Logging"
        )
      ),
      "Microsoft.NET.Sdk.WindowsDesktop" -> netSdkNamespace.appendedAll(List("System.Drawing", "System.Windows.Forms"))
    )
  }

  /** Extracts implicit usings based on the project type, i.e. based on the `<Project Sdk="XXX">` tag.
    */
  private def from(rootElem: Elem): List[String] = {
    val projectType = rootElem.label match
      case "Project" => rootElem.attribute("Sdk").flatMap(_.headOption.map(_.text))
      case _         => None

    val implicitUsingsEnabled = rootElem.child
      .collect { case x if x.label == "PropertyGroup" => x.child }
      .flatten
      .collect { case x if x.label == "ImplicitUsings" => x.text }
      .exists(x => x == "true" || x == "enable")

    val usingsFromProjectType = if (projectType.isDefined && implicitUsingsEnabled) {
      projectTypeMapping.getOrElse(projectType.get, Nil)
    } else {
      Nil
    }

    // Once we gather the initial set of implicit usings (if any) based on the project type, we
    // process ItemGroup.Using tags. The order in which we process these matters, e.g.
    //    <Using Include="System"/>
    //    <Using Remove="System"/>
    // removes "System", whereas
    //    <Using Remove="System"/>
    //    <Using Include="System"/>
    // adds "System".

    rootElem.child
      .collect { case x if x.label == "ItemGroup" => x.child }
      .flatten
      .collect { case x if x.label == "Using" => x }
      .flatten
      .foldLeft(usingsFromProjectType.toSet) { case (acc, node) =>
        if (node.attribute("Remove").isDefined) {
          node.attribute("Remove").flatMap(_.headOption.map(_.text)) match
            case None           => acc
            case Some(toRemove) => acc.excl(toRemove)
        } else if (node.attribute("Include").isDefined) {
          node.attribute("Include").flatMap(_.headOption.map(_.text)) match
            case None            => acc
            case Some(toInclude) => acc + toInclude
        } else {
          acc
        }
      }
      .toList
  }

}
