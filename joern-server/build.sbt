val ScalatraVersion = "2.6.5"
organization := "io.shiftleft"
name := "Joern Server"

resolvers += Classpaths.typesafeReleases

dependsOn(Projects.joerncli)

libraryDependencies ++= Seq(
  "io.shiftleft" % "cpg-server" % Versions.cpgVersion,

  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
  "org.scalatra" %% "scalatra-json" % ScalatraVersion,
  "org.scalatra" %% "scalatra-swagger"  % ScalatraVersion,

  "com.typesafe.akka" %% "akka-actor" % "2.5.3",
  "net.databinder.dispatch" %% "dispatch-core" % "0.13.1",

  "org.json4s"   %% "json4s-native" % "3.5.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.7.v20170914" % "container;compile",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"
)

enablePlugins(JavaAppPackaging)
enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
