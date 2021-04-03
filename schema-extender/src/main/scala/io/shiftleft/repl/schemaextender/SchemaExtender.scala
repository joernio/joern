package io.shiftleft.repl.schemaextender

import org.zeroturnaround.zip._
import better.files._
import overflowdb.codegen._
import sys.process._

/**
  * Allows to extend the default cpg schema.
  * The user-provided schema is used to generate and compile all node/edge/... classes from scratch.
  * Then, the default classes are overridden in the jar containing the old generated classes (must be passed in as parameter).
  */
object SchemaExtender extends App {
  case class Config(targetJar: File, scalacPath: String)

  def parseConfig =
    new scopt.OptionParser[Config](getClass.getSimpleName) {
      opt[String]("target").required()
        .action { (x, c) =>
          c.copy(targetJar = File(x))
        }
        .validate { path =>
          if (File(path).exists) success
          else failure(s"$path does not exist")
        }
        .text("path to target jar which contains the generated nodes")
      opt[String]("scalac").required()
        .action { (x, c) =>
          c.copy(scalacPath = x)
        }
        .text("path to scalac (used to compile the generated sources)")
        .validate { path =>
          val path0 = File(path)
          if (!path0.exists) failure(s"$path does not exist")
          else if (!path0.isExecutable) failure(s"$path is not executable")
          else success
        }
    }.parse(args, Config(null, null))

  val (targetJar: File, scalacPath: String) = parseConfig match {
    case None =>
      // bad arguments, error message will have been displayed. exiting.
      System.exit(1)
    case Some(config) => (config.targetJar, config.scalacPath)
  }

  val schemasDir = File("schemas")
  assert(schemasDir.exists, s"$schemasDir doesn't exist")

  val classesOutDir = File("generated/classes")
  val classFiles = compile(generateDomainClassSources)
  updateTargetJar(classFiles)
  println(s"finished successfully, $targetJar now contains generated classes for the schema defined in $schemasDir")

  lazy val backupJar: File = {
    val backup = File("backup.jar")
    if (!backup.exists) {
      targetJar.copyTo(backup)
      println(s"backed up original $targetJar to $backup")
    }
    backup
  }

  def generateDomainClassSources: Iterator[File] = {
    // val inputSchemaFiles = schemasDir.listRecursively.filter(_.name.endsWith(".json"))
    // val generatedSrcDir = File("generated/src")
    // val mergedSchemaFile = SchemaMerger.mergeCollections(inputSchemaFiles.map(_.toJava).toSeq)
    // new CodeGen(
    //   mergedSchemaFile.getAbsolutePath,
    //   "io.shiftleft.codepropertygraph.generated"
    // ).run(generatedSrcDir.toJava)
    // generatedSrcDir.listRecursively.filter(_.isRegularFile)
    ???
  }

  def compile(sources: Iterator[File]): Iterator[File] = {
    classesOutDir.createDirectories().children.foreach(_.delete())
    val exitCode =
      s"""$scalacPath -d $classesOutDir ${sources.mkString(" ")}""".!
    assert(exitCode == 0, s"error while invoking scalac. exit code was $exitCode")
    classesOutDir.listRecursively.filter(_.isRegularFile)
  }

  def updateTargetJar(classFiles: Iterator[File]) = {
    val newZipEntries: Array[ZipEntrySource] = classFiles.map { classFile =>
      val pathInJar = classesOutDir.relativize(classFile).toString
      new FileSource(pathInJar, classFile.toJava)
    }.toArray

    ZipUtil.addOrReplaceEntries(backupJar.toJava, newZipEntries, targetJar.toJava)
    println(s"added/replaced ${newZipEntries.size} class files in $targetJar")
  }
}
