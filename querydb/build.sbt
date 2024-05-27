name := "querydb"

enablePlugins(JavaAppPackaging)

dependsOn(
  Projects.console,
  Projects.macros,
  Projects.ghidra2cpg  % "test->test",
  Projects.javasrc2cpg % "test->test",
  Projects.kotlin2cpg  % "test->test",
  Projects.c2cpg       % "test->test",
  Projects.kotlin2cpg  % "test->test"
)

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % Versions.scalatest % Test)

topLevelDirectory := Some(name.value)

lazy val createDistribution = taskKey[File]("Create binary distribution of extension")
createDistribution := {
  import better.files._
  val pkgBin        = (Universal / packageBin).value
  val tmpDstArchive = "/tmp/querydb.zip"
  val dstArchive    = (target.value / "querydb.zip").toScala
  if (dstArchive.exists) dstArchive.delete()
  IO.copy(
    List((pkgBin, file(tmpDstArchive))),
    CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true)
  )

  File.usingTemporaryDirectory("querydb") { dir =>
    File(tmpDstArchive).unzipTo(dir)
    dir.listRecursively
      .filter { x =>
        val name = x.toString
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
      }
      .foreach(_.delete())
    dir.zipTo(dstArchive)
    File(tmpDstArchive).delete()
  }
  println(s"created distribution - resulting files: $dstArchive")

  dstArchive.toJava
}

Compile / scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-language:implicitConversions")

fork := true

maintainer := "fabs@shiftleft.io"

githubOwner      := "Privado-Inc"
githubRepository := "joern"
