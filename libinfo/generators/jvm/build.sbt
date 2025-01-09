name := "libinfogenjvm"

dependsOn(
  Projects.libinfo
)

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "com.github.scopt"       %% "scopt"             % Versions.scopt,
  "org.ow2.asm"             % "asm"               % "9.7.1",
  "org.scalatest"          %% "scalatest"         % Versions.scalatest % Test
)
