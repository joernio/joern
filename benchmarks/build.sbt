name := "benchmarks"

crossScalaVersions := Seq("2.13.8", "3.1.2")

dependsOn(Projects.dataflowengineoss)
dependsOn(Projects.semanticcpg)
dependsOn(Projects.console)
dependsOn(Projects.x2cpg)
dependsOn(Projects.joerncli)
dependsOn(Projects.javasrc2cpg)
dependsOn(Projects.jimple2cpg)

libraryDependencies ++= Seq(
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % Versions.log4j     % Test,
  "org.scalatest"           %% "scalatest"        % Versions.scalatest % Test
)

Compile / doc / sources ~= (_ filter (_ => false))

// we want to consume this from a java8 build
compile / javacOptions ++= Seq("--release", "8")
scalacOptions ++= Seq() ++ (
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq("-Xtarget:8")
    case _            => Seq("-target:jvm-1.8")
  }
)

trapExit    := false
Test / fork := true
