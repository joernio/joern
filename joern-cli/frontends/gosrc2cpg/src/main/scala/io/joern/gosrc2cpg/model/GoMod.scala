package io.joern.gosrc2cpg.model

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.utils.UtilityConstants.fileSeparateorPattern

class GoModHelper(config: Option[Config] = None, meta: Option[GoMod] = None) {

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

      val remainingpath = compilationUnitFilePath.stripPrefix(config.get.inputPath)
      val pathTokens    = remainingpath.split(fileSeparateorPattern)
      val tokens        = pathTokens.dropRight(1).filterNot(x => x == null || x.trim.isEmpty) :+ pkg
      return tokens.mkString("/")
    }

    // go.mod (module jorn.io/trial) and <root project path>/foo.go (package trial) => jorn.io/trial
    // go.mod (module jorn.io/trial) and <root project path>/foo.go (package foo) => jorn.io/trial>foo
    // go.mod (module jorn.io/trial) and <root project path>/first/foo.go (package first) => jorn.io/trial/first
    // go.mod (module jorn.io/trial) and <root project path>/first/foo.go (package bar) => jorn.io/trial/first
    val remainingpath = compilationUnitFilePath.stripPrefix(config.get.inputPath)
    val pathTokens    = remainingpath.split(fileSeparateorPattern)
    // prefixing module name i.e. jorn.io/trial
    val tokens = meta.get.module.name +: pathTokens.dropRight(1).filterNot(x => x == null || x.trim.isEmpty)
    tokens.mkString("/")
  }
}

case class GoMod(fileFullPath: String, module: GoModModule, dependencies: List[GoModDependency])
case class GoModModule(
  name: String,
  lineNo: Option[Int] = None,
  colNo: Option[Int] = None,
  endLineNo: Option[Int] = None,
  endColNo: Option[Int] = None
)
case class GoModDependency(
  module: String,
  version: String,
  indirect: Boolean,
  lineNo: Option[Int] = None,
  colNo: Option[Int] = None,
  endLineNo: Option[Int] = None,
  endColNo: Option[Int] = None
)

object CirceEnDe {
  implicit val decoderModModule: Decoder[GoModModule] = new Decoder[GoModModule] {
    override def apply(c: HCursor): Result[GoModModule] = {
      val name      = c.downField("Name").as[String]
      val lineNo    = c.downField("node_line_no").as[Int]
      val endLineNo = c.downField("node_line_no_end").as[Int]
      val colNo     = c.downField("node_col_no").as[Int]
      val endColNo  = c.downField("node_col_no_end").as[Int]
      Right(
        GoModModule(
          name = name.getOrElse(""),
          lineNo = lineNo.toOption,
          colNo = colNo.toOption,
          endLineNo = endLineNo.toOption,
          endColNo = endColNo.toOption
        )
      )
    }
  }
  implicit val decoderModDependency: Decoder[GoModDependency] = new Decoder[GoModDependency] {
    override def apply(c: HCursor): Result[GoModDependency] = {
      val module    = c.downField("Module").as[String]
      val version   = c.downField("Version").as[String]
      val indirect  = c.downField("Indirect").as[Boolean]
      val lineNo    = c.downField("node_line_no").as[Int]
      val endLineNo = c.downField("node_line_no_end").as[Int]
      val colNo     = c.downField("node_col_no").as[Int]
      val endColNo  = c.downField("node_col_no_end").as[Int]
      Right(
        GoModDependency(
          module = module.getOrElse(""),
          version = version.getOrElse(""),
          indirect = indirect.getOrElse(false),
          lineNo = lineNo.toOption,
          colNo = colNo.toOption,
          endLineNo = endLineNo.toOption,
          endColNo = endColNo.toOption
        )
      )
    }
  }

  implicit val decoderModMetadata: Decoder[GoMod] = new Decoder[GoMod] {
    override def apply(c: HCursor): Result[GoMod] = {
      val fileName     = c.downField("node_filename").as[String]
      val module       = c.downField("Module").as[GoModModule]
      val dependencies = c.downField("dependencies").as[List[GoModDependency]]
      Right(
        GoMod(
          fileFullPath = fileName.getOrElse(""),
          module = module.getOrElse(null),
          dependencies = dependencies.getOrElse(List[GoModDependency]())
        )
      )
    }
  }
}
