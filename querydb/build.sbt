name := "querydb"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "sourcecode" % "0.1.9",
  "com.lihaoyi" %% "upickle" % "1.2.2",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "io.joern" %% "ghidra2cpg" % Versions.ghidra2cpg,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg,
  "io.shiftleft" %% "console" % Versions.cpg,
  "io.shiftleft" %% "dataflowengineoss" % Versions.cpg,
  "io.shiftleft" %% "fuzzyc2cpg-tests" % Versions.cpg % Test classifier "tests",
  "io.shiftleft" %% "c2cpg-tests" % Versions.cpg % Test classifier "tests",
  "io.shiftleft" %% "semanticcpg-tests" % Versions.cpg % Test classifier "tests",
  "io.shiftleft" %% "fuzzyc2cpg" % Versions.cpg % Test,
  "io.shiftleft" %% "c2cpg" % Versions.cpg % Test,
  "org.scalatest" %% "scalatest" % "3.1.1" % Test,
  "io.joern" %% "ghidra2cpg-tests" % Versions.ghidra2cpg % Test classifier "tests"
)

Compile/doc/sources := Seq.empty
Compile/packageDoc/publishArtifact := false

lazy val createDistribution = taskKey[File]("Create binary distribution of extension")
createDistribution := {
  val pkgBin = (Universal/packageBin).value
  val tmpDstArchive = "/tmp/querydb.zip"
  val dstArchive = "querydb.zip"
  IO.copy(
    List((pkgBin, file(tmpDstArchive))),
    CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true)
  )

  val f = better.files.File(dstArchive)
  better.files.File.usingTemporaryDirectory("querydb") { dir =>
    better.files.File(tmpDstArchive).unzipTo(dir)
    dir.listRecursively.filter{ x => val name = x.toString
        name.contains("org.scala") ||
        name.contains("net.sf.trove4") ||
        name.contains("com.google.guava") ||
        name.contains("org.apache.logging") ||
        name.contains("com.google.protobuf") ||
        name.contains("com.lihaoyi") ||
        name.contains("io.shiftleft") ||
        name.contains("org.typelevel") ||
        name.contains("io.undertow") ||
        name.contains("com.chuusai") ||
        name.contains("io.get-coursier") ||
        name.contains("io.circe") ||
        name.contains("net.java.dev") ||
        name.contains("com.github.javaparser") ||
        name.contains("org.javassist") ||
        name.contains("com.lihaoyi.ammonite") ||
        name.contains("io.joern.ghidra2cpg") ||
        name.contains("net.oneandone")
    }.foreach(x => x.delete())
    dir.zipTo(f)
    better.files.File(tmpDstArchive).delete()
  }

  println(s"created distribution - resulting files: $dstArchive")
  f.toJava
}

Compile/scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-feature",
  "-deprecation",
  "-language:implicitConversions",
)

fork := true
