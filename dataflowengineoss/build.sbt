name := "dataflowengineoss"

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "semanticcpg"    % Versions.cpg,
  "org.antlr"     %  "antlr4-runtime" % Versions.antlr,
  "org.scalatest" %% "scalatest"      % Versions.scalatest % Test,
)

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("io.shiftleft.dataflowengineoss")
Antlr4 / antlr4Version := Versions.antlr
Antlr4 / javaSource := (Compile / sourceManaged).value
Compile/doc/sources ~= (_ filter (_ => false))
