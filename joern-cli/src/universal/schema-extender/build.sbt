import java.nio.file.Files
import scala.jdk.CollectionConverters.*

name := "schema-extender"

ThisBuild / scalaVersion := "3.8.3"

val cpgVersion = IO.read(file("cpg-version"))

val generateDomainClasses = taskKey[Seq[File]]("generate domain classes for our schema")

val joernInstallPath =
  settingKey[String]("path to joern installation, e.g. `/home/username/bin/joern/joern-cli` or `../../joern/joern-cli`")
joernInstallPath := "../"

val replaceDomainClassesInJoern =
  taskKey[Unit]("generates new domain classes based on the given schema, and installs them in the joern distribution")

// the implicit root project aggregates the subprojects; without this, the task below would run once per
// project and the concurrent jar copies would race each other
replaceDomainClassesInJoern / aggregate := false

replaceDomainClassesInJoern := Def.uncached {
  import java.nio.file._
  val converter          = fileConverter.value
  val newDomainClassesJar = converter.toPath((domainClasses / Compile / packageBin).value).toFile

  val targetFile =
    file(joernInstallPath.value) / "lib" / s"io.shiftleft.codepropertygraph-domain-classes_3-$cpgVersion.jar"
  assert(targetFile.exists, s"target jar assumed to be $targetFile, but that file doesn't exist...")

  println(s"copying $newDomainClassesJar to $targetFile")
  Files.copy(newDomainClassesJar.toPath, targetFile.toPath, StandardCopyOption.REPLACE_EXISTING)
}

ThisBuild / libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph-schema"         % cpgVersion,
  "io.shiftleft" %% "codepropertygraph-domain-classes" % cpgVersion
)

val codegenOutputRoot = file("target/fg-codegen")
lazy val schema = project
  .in(file("schema"))
  .settings(
    name := "schema",
    generateDomainClasses := Def.uncached {
      // plain NIO listing on purpose: sbt 2's IO/Path utilities don't reliably observe the files
      // written by the forked codegen run (virtual io). Also note: we deliberately don't clean
      // codegenOutputRoot first - deleting the directory has the same visibility problem.
      val invoked = (Compile / runMain).toTask(s" CpgExtCodegen ${codegenOutputRoot.getAbsolutePath}").value
      Files.walk(codegenOutputRoot.toPath).iterator.asScala.map(_.toFile).filter(!_.isDirectory).toSeq
    }
  )

lazy val domainClasses =
  project
    .in(file("domain-classes"))
    .settings(
      name := "domain-classes",
      Compile / sourceGenerators += schema / generateDomainClasses
    )

Global / onChangedBuildSource := ReloadOnSourceChanges
