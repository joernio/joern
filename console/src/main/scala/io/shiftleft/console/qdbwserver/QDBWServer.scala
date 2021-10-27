package io.shiftleft.console.qdbwserver

import cask.MainRoutes

class QDBWServer(serverHost: String, serverPort: Int, contentDir: String) extends MainRoutes {

  override def port: Int = {
    serverPort
  }

  override def host: String = {
    serverHost
  }

  @cask.staticFiles("/")
  def staticFileRoutes() = contentDir

  initialize()
}
