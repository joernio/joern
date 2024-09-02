package io.joern.x2cpg.utils.server

import java.time.Duration

object FrontendHTTPDefaults {

  val host: String      = "localhost"
  val port: Int         = 9000
  val timeout: Duration = Duration.ofSeconds(5)
  val route: String     = "run"

}
