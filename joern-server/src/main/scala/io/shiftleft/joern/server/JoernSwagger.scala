package io.shiftleft.joern.server

import org.scalatra.ScalatraServlet
import org.scalatra.swagger.{ApiInfo, NativeSwaggerBase, Swagger}

class ResourcesApp(implicit val swagger: Swagger) extends ScalatraServlet with NativeSwaggerBase

object JoernApiInfo
    extends ApiInfo("The Joern API",
                    "Docs for the Joern API",
                    "http://joern.io",
                    "fabs@codeminers.org",
                    "Apache2",
                    "https://www.apache.org/licenses/LICENSE-2.0")

class JoernSwagger extends Swagger(Swagger.SpecVersion, "1.0.0", JoernApiInfo)
