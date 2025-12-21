// Require Java 13+ due to FileSystems.newFileSystem(Path) API used in project/FileUtils.scala
// This method signature was added in JDK13
initialize := {
  val _ = initialize.value
  val javaVersion = sys.props("java.specification.version").toFloat
  assert(javaVersion.toInt >= 13, s"this build requires JDK13+ - you're using $javaVersion")
}

libraryDependencies ++= Seq(
  "com.typesafe"          % "config"       % "1.4.3",
  "com.michaelpollmeier"  % "versionsort"  % "1.0.11",
  "net.lingala.zip4j"     % "zip4j"        % "2.11.5",
  "com.github.pathikrit" %% "better-files" % "3.9.2",
  "net.java.dev.javacc"   % "javacc"       % "7.0.12",
  "com.lihaoyi"          %% "os-lib"       % "0.10.1"
)
