import sbt.BareBuildSyntax.dependsOn

name := "joern-cli"

dependsOn(
  Projects.console,
  Projects.console % "test->test",
  Projects.dataflowengineoss,
  Projects.x2cpg,
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"     %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"      %% "requests"          % Versions.requests,
  "com.lihaoyi"      %% "upickle"           % Versions.upickle,
  "com.github.scopt" %% "scopt"             % Versions.scopt,
  "org.reflections"   % "reflections"       % Versions.reflection,
  "org.scalatest"    %% "scalatest"         % Versions.scalatest % Test
)

// See console/build.sbt for why there is intentionally no `Test / compile` dependsOn `stage` edge.
Test / fork := false

enablePlugins(JavaAppPackaging, UniversalPlugin)

//wildcard import from staged `lib` dir, for simplicity and also to avoid `line too long` error on windows
scriptClasspath := Seq("*")

topLevelDirectory := Some(packageName.value)

Compile / packageDoc / mappings := Seq()

def frontendMappings(
    frontendName: String,
    stagedProject: File,
    conv: xsbti.FileConverter
): Seq[(xsbti.HashedVirtualFileRef, String)] =
  NativePackagerHelper.contentOf(stagedProject, conv).map { case (ref, name) =>
    ref -> s"frontends/$frontendName/$name"
  }

lazy val x2cpg         = project.in(file("frontends/x2cpg"))
lazy val abap2cpg      = project.in(file("frontends/abap2cpg"))
lazy val kotlin2cpg    = project.in(file("frontends/kotlin2cpg"))
lazy val javasrc2cpg   = project.in(file("frontends/javasrc2cpg"))
lazy val pysrc2cpg     = project.in(file("frontends/pysrc2cpg"))
lazy val php2cpg       = project.in(file("frontends/php2cpg"))
lazy val jimple2cpg    = project.in(file("frontends/jimple2cpg"))
lazy val jssrc2cpg     = project.in(file("frontends/jssrc2cpg"))
lazy val swiftsrc2cpg  = project.in(file("frontends/swiftsrc2cpg"))
lazy val rubysrc2cpg   = project.in(file("frontends/rubysrc2cpg"))
lazy val gosrc2cpg     = project.in(file("frontends/gosrc2cpg"))
lazy val csharpsrc2cpg = project.in(file("frontends/csharpsrc2cpg"))
lazy val rust2cpg      = project.in(file("frontends/rust2cpg"))

Universal / mappings ++= frontendMappings("kotlin2cpg",   (kotlin2cpg / stage).value,          fileConverter.value)
Universal / mappings ++= frontendMappings("abap2cpg",     (abap2cpg / stage).value,             fileConverter.value)
Universal / mappings ++= frontendMappings("javasrc2cpg",  (javasrc2cpg / stage).value,          fileConverter.value)
Universal / mappings ++= frontendMappings("c2cpg",        (Projects.c2cpg / stage).value,       fileConverter.value)
Universal / mappings ++= frontendMappings("ghidra2cpg",   (Projects.ghidra2cpg / stage).value,  fileConverter.value)
Universal / mappings ++= frontendMappings("jssrc2cpg",    (jssrc2cpg / stage).value,            fileConverter.value)
Universal / mappings ++= frontendMappings("swiftsrc2cpg", (swiftsrc2cpg / stage).value,         fileConverter.value)
Universal / mappings ++= frontendMappings("jimple2cpg",   (jimple2cpg / stage).value,           fileConverter.value)
Universal / mappings ++= frontendMappings("pysrc2cpg",    (pysrc2cpg / stage).value,            fileConverter.value)
Universal / mappings ++= frontendMappings("php2cpg",      (php2cpg / stage).value,              fileConverter.value)
Universal / mappings ++= frontendMappings("rubysrc2cpg",  (rubysrc2cpg / stage).value,          fileConverter.value)
Universal / mappings ++= frontendMappings("gosrc2cpg",    (gosrc2cpg / stage).value,            fileConverter.value)
Universal / mappings ++= frontendMappings("csharpsrc2cpg",(csharpsrc2cpg / stage).value,        fileConverter.value)
Universal / mappings ++= frontendMappings("rust2cpg",     (rust2cpg / stage).value,             fileConverter.value)

lazy val cpgVersionFile = taskKey[File]("persist cpg version in file (e.g. for schema-extender)")
cpgVersionFile := Def.uncached {
  val ret = target.value / "cpg-version"
  better.files
    .File(ret.getPath)
    .createIfNotExists(createParents = true)
    .writeText(Versions.cpg)
  ret
}
Universal / mappings += {
  val conv = fileConverter.value
  conv.toVirtualFile(cpgVersionFile.value.toPath) -> "schema-extender/cpg-version"
}

lazy val extractScaladocSources = taskKey[Seq[File]]("Extract scaladoc sources from CPG and semanticcpg JARs")
extractScaladocSources := Def.uncached {
  import net.lingala.zip4j.ZipFile

  val converter     = fileConverter.value
  val updateReport  = updateClassifiers.value
  val inputFilesDir = target.value / "inputFiles"

  if (inputFilesDir.exists) IO.delete(inputFilesDir)
  IO.createDirectory(inputFilesDir)

  // codepropertygraph is an external Maven dep — fetch its sources JAR via updateClassifiers
  val cpgJar = SbtHelper.findJar("codepropertygraph_3", updateReport, SbtHelper.JarClassifier.Sources)
  new ZipFile(cpgJar).extractAll(inputFilesDir.getAbsolutePath)
  // semanticcpg is a local project dep — get its sources JAR directly from packageSrc
  val semanticcpgSrcRef = (Projects.semanticcpg / Compile / packageSrc).value
  new ZipFile(converter.toPath(semanticcpgSrcRef).toFile).extractAll(inputFilesDir.getAbsolutePath)

  FileUtils.listFilesRecursively(inputFilesDir)
    .filter(f => f.getName.endsWith(".java") || f.getName.endsWith(".scala"))
}

Compile / doc / sources := extractScaladocSources.value

Compile / doc / scalacOptions ++= {
  val converter  = fileConverter.value
  val xapisFiles = apiMappings.value.map { case (ref, uri) => converter.toPath(ref).toFile -> uri }.toSeq
  Seq("-language:implicitConversions", "-doc-root-content", "api-doc-root.txt", "-implicits") ++
    Opts.doc.externalAPIScala3(xapisFiles)
}

lazy val generateScaladocs = taskKey[File]("generate scaladocs from combined project sources")
generateScaladocs := Def.uncached { (Compile / doc).value }

Universal / mappings ++= {
  val conv = fileConverter.value
  // absolute base: sbt 2 warns about (and mishandles caching of) relative globs
  sbt.Path.directory(baseDirectory.value / "src" / "main" / "resources" / "scripts").map { case (f, name) =>
    conv.toVirtualFile(f.toPath) -> name
  }
}

// remove module-info.class from dependency jars - a hacky workaround for a scala3 compiler bug
// see https://github.com/scala/scala3/issues/20421
val moduleInfoLocation = "module-info.class"
Universal / mappings := {
  val conv      = fileConverter.value
  val targetDir = target.value
  val log       = streams.value.log
  (Universal / mappings).value.map {
    case (jarRef, location) if location.startsWith("lib") && location.endsWith(".jar") =>
      val jar = conv.toPath(jarRef).toFile
      if (FileUtils.jarContainsEntryInRoot(jar, moduleInfoLocation)) {
        val newJar = targetDir / "without-module-info" / jar.getName
        IO.copyFile(jar, newJar)
        FileUtils.removeJarEntryFromRoot(newJar, moduleInfoLocation)
        log.info(
          s"workaround for scala completion bug: including a modified version of $jar without the $moduleInfoLocation entry: $newJar"
        )
        conv.toVirtualFile(newJar.toPath) -> location
      } else {
        jarRef -> location
      }
    case other => other
  }
}

maintainer := "fabs@shiftleft.io"
