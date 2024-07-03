package io.joern.gosrc2cpg.model

import io.joern.gosrc2cpg.utils.UtilityConstants.fileSeparateorPattern
import upickle.default.*

import java.util
import java.util.Set
import java.util.concurrent.ConcurrentSkipListSet
import scala.util.control.Breaks.*

class GoModHelper(modulePath: Option[String] = None, meta: Option[GoMod] = None) {

  def getModMetaData(): Option[GoMod] = meta
  def getNameSpace(compilationUnitFilePath: String, pkg: String): String = {
    if (meta.isEmpty || compilationUnitFilePath == null || compilationUnitFilePath.isEmpty) {
      // When there no go.mod file, we don't have the information about the module prefix
      // In this case we will use package name as a namespace
      return pkg
    } else if (pkg == "main") {
      // 'main' in go is specially treated package. One write the code in this package for entry point.
      // One cannot import and use the exported methods and variables from this package.
      // Having said that one can define a 'main' within any folder structure.
      // In order to isolate the main package from the root folder main package. We are using folder structure from
      // root project path appended with main
      //
      // e.g.
      // 1. if there is go file inside <root project path>/first/second/test.go (package main) => '/first/second/main'
      // 2. <root project path>/test.go (package main) => 'main'

      val remainingpath = compilationUnitFilePath.stripPrefix(modulePath.get)
      val pathTokens    = remainingpath.split(fileSeparateorPattern)
      val tokens        = pathTokens.dropRight(1).filterNot(x => x == null || x.trim.isEmpty) :+ pkg
      return tokens.mkString("/")
    }

    // go.mod (module jorn.io/trial) and <root project path>/foo.go (package trial) => jorn.io/trial
    // go.mod (module jorn.io/trial) and <root project path>/foo.go (package foo) => jorn.io/trial>foo
    // go.mod (module jorn.io/trial) and <root project path>/first/foo.go (package first) => jorn.io/trial/first
    // go.mod (module jorn.io/trial) and <root project path>/first/foo.go (package bar) => jorn.io/trial/first
    val remainingpath = compilationUnitFilePath.stripPrefix(modulePath.get)
    val pathTokens    = remainingpath.split(fileSeparateorPattern)
    // prefixing module name i.e. jorn.io/trial
    val tokens = meta.get.module.name +: pathTokens.dropRight(1).filterNot(x => x == null || x.trim.isEmpty)
    tokens.mkString("/")
  }

  def recordUsedDependencies(importStmt: String): Unit = {
    breakable {
      meta.map(mod =>
        // TODO: && also add a check for builtin package imports to skip those
        if (!importStmt.startsWith(mod.module.name)) {
          for (dependency <- mod.dependencies) {
            if (importStmt.startsWith(dependency.module)) {
              dependency.beingUsed = true
              dependency.usedPackages.add(importStmt.replace(dependency.module, ""))
            }
          }
        }
      )
    }
  }
}

case class GoMod(
  @upickle.implicits.key("node_filename") fileFullPath: String = "",
  @upickle.implicits.key("Module") module: GoModModule,
  @upickle.implicits.key("dependencies") dependencies: List[GoModDependency] = List.empty
) derives ReadWriter

implicit val goModModuleRw: ReadWriter[GoModModule] = readwriter[ujson.Value].bimap[GoModModule](
  x =>
    ujson.Obj(
      "Name"             -> x.name,
      "node_line_no"     -> x.lineNo.getOrElse(-1),
      "node_col_no"      -> x.colNo.getOrElse(-1),
      "node_line_no_end" -> x.endLineNo.getOrElse(-1),
      "node_col_no_end"  -> x.endColNo.getOrElse(-1)
    ),
  json =>
    GoModModule(
      name = json("Name").strOpt.getOrElse(""),
      lineNo = json("node_line_no").numOpt.map(_.toInt),
      colNo = json("node_col_no").numOpt.map(_.toInt),
      endLineNo = json("node_line_no_end").numOpt.map(_.toInt),
      endColNo = json("node_col_no_end").numOpt.map(_.toInt)
    )
)

case class GoModModule(
  name: String,
  lineNo: Option[Int] = None,
  colNo: Option[Int] = None,
  endLineNo: Option[Int] = None,
  endColNo: Option[Int] = None
)

implicit val goModDependencyRw: ReadWriter[GoModDependency] = readwriter[ujson.Value].bimap[GoModDependency](
  x =>
    ujson.Obj(
      "Module"           -> x.module,
      "Version"          -> x.version,
      "Indirect"         -> x.indirect,
      "node_line_no"     -> x.lineNo.getOrElse(-1),
      "node_col_no"      -> x.colNo.getOrElse(-1),
      "node_line_no_end" -> x.endLineNo.getOrElse(-1),
      "node_col_no_end"  -> x.endColNo.getOrElse(-1)
    ),
  json =>
    GoModDependency(
      module = json("Module").strOpt.getOrElse(""),
      version = json("Version").strOpt.getOrElse(""),
      indirect = json("Indirect").boolOpt.getOrElse(false),
      lineNo = json("node_line_no").numOpt.map(_.toInt),
      colNo = json("node_col_no").numOpt.map(_.toInt),
      endLineNo = json("node_line_no_end").numOpt.map(_.toInt),
      endColNo = json("node_col_no_end").numOpt.map(_.toInt)
    )
)

case class GoModDependency(
  module: String,
  version: String,
  indirect: Boolean = false,
  var beingUsed: Boolean = false,
  lineNo: Option[Int] = None,
  colNo: Option[Int] = None,
  endLineNo: Option[Int] = None,
  endColNo: Option[Int] = None,
  usedPackages: util.Set[String] = new ConcurrentSkipListSet[String]()
) {
  def getIncludePackageRegex(): String = usedPackages.toArray.mkString("(", "|", ")")
  def getIncludePackagesList(): String = usedPackages.toArray.mkString(",")
  def dependencyStr(): String          = s"$module@$version"
}

implicit val javaSetRw: ReadWriter[util.Set[String]] = {
  import scala.jdk.CollectionConverters.*

  readwriter[ujson.Value]
    .bimap[util.Set[String]](
      x => ujson.Arr(x.asScala.map(ujson.Str.apply).toSeq*),
      json => json.arr.map(_.str).toSet.asJava
    )
}
