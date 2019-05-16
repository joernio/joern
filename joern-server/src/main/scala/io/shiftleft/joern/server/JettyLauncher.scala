package io.shiftleft.joern.server

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.slf4j.LoggerFactory

/**
  * Launcher to enable running of cpg-server as a
  * standalone program
  * */
object JettyLauncher extends App {

  val logger = LoggerFactory.getLogger(getClass)

  val port = if (System.getenv("JOERN_PORT") != null) System.getenv("PORT").toInt else 8080
  val server = new Server(port)
  val context = new WebAppContext()
  context setContextPath "/"
  context.setResourceBase("src/main/webapp")
  context.addEventListener(new ScalatraListener)
  context.addServlet(classOf[DefaultServlet], "/")

  server.setHandler(context)

  try {
    server.start
  } catch {
    case exception: java.net.BindException => {
      logger.warn(s"Not starting server - port $port is occupied")
      System.exit(1)
    }
  }

  server.join

}
