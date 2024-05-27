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

trapExit    := false
Test / fork := true

credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
