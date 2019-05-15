package io.shiftleft.cpgserver

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

/**
  * Launcher to enable running of joern-server as a
  * standalone program
  * */
object JettyLauncher extends App {

  val port = if (System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080
  val server = new Server(port)
  val context = new WebAppContext()
  context setContextPath "/"
  context.setResourceBase("src/main/webapp")
  context.addEventListener(new ScalatraListener)
  context.addServlet(classOf[DefaultServlet], "/")

  server.setHandler(context)
  server.start
  server.join

}
