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

// Output directory for the generated domain classes. Declared at build level because sbt 2's `toTask`
// inlines its argument into a separate task definition, which cannot capture task-local vals.
val codegenOutputRoot = file("target/fg-codegen")

lazy val schema = project
  .in(file("schema"))
  .settings(
    // sbt 2 derives each project's output directory from its name and rejects overlaps; without an explicit
    // name the subprojects inherit the build-level `name` above and all three projects collide
    name := "schema",
    generateDomainClasses := Def.uncached {
      FileUtils.deleteRecursively(codegenOutputRoot)
      val invoked = (Compile / runMain).toTask(s" CpgExtCodegen ${codegenOutputRoot.getAbsolutePath}").value
      FileUtils.listFilesRecursively(codegenOutputRoot)
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
