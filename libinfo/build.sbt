name := "libinfo"

libraryDependencies ++= Seq(
  "com.amazon.ion"          % "ion-java"            % "1.11.9",
  "org.typelevel"          %% "cats-effect"         % "3.5.7",
  "org.typelevel"          %% "log4cats-slf4j"      % "2.7.0",
  "co.fs2"                 %% "fs2-io"              % "3.11.0",
  "org.scala-lang.modules" %% "scala-xml"           % "2.2.0",
  "org.ow2.asm"             % "asm"                 % "9.7.1",
  "org.apache.logging.log4j" % "log4j-slf4j2-impl"  % Versions.log4j,
  "org.http4s"             %% "http4s-ember-client" % Versions.http4s,
  "org.http4s"             %% "http4s-ember-server" % Versions.http4s,
  "org.http4s"             %% "http4s-dsl"          % Versions.http4s,
  "org.scalatest"          %% "scalatest"           % Versions.scalatest % Test,
)

enablePlugins(JavaAppPackaging)
