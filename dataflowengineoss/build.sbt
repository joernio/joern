name := "dataflowengineoss"

scalaVersion := "2.13.7"
crossScalaVersions := Seq("2.13.7", "3.1.0")
libraryDependencies ++= Seq(
  "io.shiftleft"  %% "semanticcpg"    % Versions.cpg,
  "org.antlr"     %  "antlr4-runtime" % Versions.antlr,
  "org.scalatest" %% "scalatest"      % Versions.scalatest % Test,
)

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("io.joern.dataflowengineoss")
Antlr4 / antlr4Version := Versions.antlr
Antlr4 / javaSource := (Compile / sourceManaged).value
Compile/doc/sources ~= (_ filter (_ => false))

// we want to consume this from a java8 build
compile / javacOptions ++= Seq("--release", "8")
scalacOptions ++= Seq( ) ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq(
        "-Xtarget:8",
        )
    case _ => Seq(
        "-target:jvm-1.8",
        )   
    }
    )

