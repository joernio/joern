name := "libinfo"

libraryDependencies ++= Seq(
  "com.amazon.ion"          % "ion-java"          % "1.11.9",
  "org.scalatest"          %% "scalatest"         % Versions.scalatest % Test
)