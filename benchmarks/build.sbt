name := "benchmarks"

dependsOn(Projects.dataflowengineoss)
dependsOn(Projects.semanticcpg)
dependsOn(Projects.console)
dependsOn(Projects.x2cpg)
dependsOn(Projects.joerncli)
dependsOn(Projects.javasrc2cpg)
dependsOn(Projects.jimple2cpg)

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % Versions.scalatest % Test)

Compile / doc / sources ~= (_ filter (_ => false))

// we want to consume this from a java8 build
compile / javacOptions ++= Seq("--release", "8")

scalacOptions += "-Xtarget:8"

trapExit    := false
Test / fork := true
