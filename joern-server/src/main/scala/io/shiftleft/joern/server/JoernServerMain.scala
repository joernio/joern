package io.shiftleft.joern.server

import io.shiftleft.cpgserver.JettyLauncher

/**
  * Launcher to enable running of cpg-server as a
  * standalone program
  * */
object JoernServerMain extends App {

  val port = if (System.getenv("JOERN_PORT") != null) System.getenv("PORT").toInt else 8080
  JettyLauncher.startServer(port)

}
