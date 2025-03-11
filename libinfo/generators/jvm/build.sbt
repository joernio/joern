name := "libinfogenjvm"

dependsOn(
  Projects.libinfo
)

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "com.github.scopt"       %% "scopt"             % Versions.scopt,
)
