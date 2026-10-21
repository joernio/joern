def frontendMappings(
    frontendName: String,
    stagedProject: File,
    conv: xsbti.FileConverter
): Seq[(xsbti.HashedVirtualFileRef, String)] =
  NativePackagerHelper.contentOf(stagedProject, conv).map { case (ref, name) =>
    ref -> s"frontends/$frontendName/$name"
  }

lazy val cpgVersionFile = taskKey[File]("persist cpg version in file (e.g. for schema-extender)")

lazy val extractScaladocSources = taskKey[Seq[File]]("Extract scaladoc sources from CPG and semanticcpg JARs")

lazy val generateScaladocs = taskKey[File]("generate scaladocs from combined project sources")

lazy val joerncli = (project in file("."))
  .dependsOn(
    Projects.console,
    Projects.console % "test->test",
    Projects.dataflowengineoss,
    Projects.x2cpg,
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, UniversalPlugin)
  .settings(
    name := "joern-cli",

    libraryDependencies ++= Seq(
      "io.shiftleft"     %% "codepropertygraph" % Versions.cpg,
      "com.lihaoyi"      %% "requests"          % Versions.requests,
      "com.lihaoyi"      %% "upickle"           % Versions.upickle,
      "com.github.scopt" %% "scopt"             % Versions.scopt,
      "org.reflections"   % "reflections"       % Versions.reflection,
      "org.scalatest"    %% "scalatest"         % Versions.scalatest % Test
    ),

    //wildcard import from staged `lib` dir, for simplicity and also to avoid `line too long` error on windows
    scriptClasspath := Seq("*"),

    topLevelDirectory := Some(packageName.value),

    Compile / packageDoc / mappings := Seq(),

    Universal / mappings ++= frontendMappings("kotlin2cpg",   (Projects.kotlin2cpg / stage).value,          fileConverter.value),
    Universal / mappings ++= frontendMappings("abap2cpg",     (Projects.abap2cpg / stage).value,             fileConverter.value),
    Universal / mappings ++= frontendMappings("javasrc2cpg",  (Projects.javasrc2cpg / stage).value,          fileConverter.value),
    Universal / mappings ++= frontendMappings("c2cpg",        (Projects.c2cpg / stage).value,       fileConverter.value),
    Universal / mappings ++= frontendMappings("ghidra2cpg",   (Projects.ghidra2cpg / stage).value,  fileConverter.value),
    Universal / mappings ++= frontendMappings("jssrc2cpg",    (Projects.jssrc2cpg / stage).value,            fileConverter.value),
    Universal / mappings ++= frontendMappings("swiftsrc2cpg", (Projects.swiftsrc2cpg / stage).value,         fileConverter.value),
    Universal / mappings ++= frontendMappings("jimple2cpg",   (Projects.jimple2cpg / stage).value,           fileConverter.value),
    Universal / mappings ++= frontendMappings("pysrc2cpg",    (Projects.pysrc2cpg / stage).value,            fileConverter.value),
    Universal / mappings ++= frontendMappings("php2cpg",      (Projects.php2cpg / stage).value,              fileConverter.value),
    Universal / mappings ++= frontendMappings("rubysrc2cpg",  (Projects.rubysrc2cpg / stage).value,          fileConverter.value),
    Universal / mappings ++= frontendMappings("gosrc2cpg",    (Projects.gosrc2cpg / stage).value,            fileConverter.value),
    Universal / mappings ++= frontendMappings("csharpsrc2cpg",(Projects.csharpsrc2cpg / stage).value,        fileConverter.value),
    Universal / mappings ++= frontendMappings("rust2cpg",     (Projects.rust2cpg / stage).value,             fileConverter.value),
    cpgVersionFile := Def.uncached {
      val ret = target.value / "cpg-version"
      better.files
        .File(ret.getPath)
        .createIfNotExists(createParents = true)
        .writeText(Versions.cpg)
      ret
    },
    Universal / mappings += {
      val conv = fileConverter.value
      conv.toVirtualFile(cpgVersionFile.value.toPath) -> "schema-extender/cpg-version"
    },
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

      sbt.Path
        .directory(inputFilesDir)
        .map(_._1)
        .filter(f => f.getName.endsWith(".java") || f.getName.endsWith(".scala"))
    },

    Compile / doc / sources := extractScaladocSources.value,

    Compile / doc / scalacOptions ++= {
      val converter  = fileConverter.value
      val xapisFiles = apiMappings.value.map { case (ref, uri) => converter.toPath(ref).toFile -> uri }.toSeq
      Seq("-language:implicitConversions", "-doc-root-content", "api-doc-root.txt", "-implicits") ++
        Opts.doc.externalAPIScala3(xapisFiles)
    },
    generateScaladocs := Def.uncached { (Compile / doc).value },

    Universal / mappings ++= {
      val conv = fileConverter.value
      // absolute base: sbt 2 warns about (and mishandles caching of) relative globs
      sbt.Path.directory(baseDirectory.value / "src" / "main" / "resources" / "scripts").map { case (f, name) =>
        conv.toVirtualFile(f.toPath) -> name
      }
    },

    maintainer := "fabs@shiftleft.io"
  )
