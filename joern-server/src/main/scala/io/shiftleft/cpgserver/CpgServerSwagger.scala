package io.shiftleft.cpgserver

import org.scalatra.ScalatraServlet
import org.scalatra.swagger.{ApiInfo, NativeSwaggerBase, Swagger}

class ResourcesApp(implicit val swagger: Swagger) extends ScalatraServlet with NativeSwaggerBase

object CpgServerApiInfo
    extends ApiInfo("The CPG Server API",
                    "Docs for the CPG Server API",
                    "http://joern.io",
                    "fabs@codeminers.org",
                    "Apache2",
                    "https://www.apache.org/licenses/LICENSE-2.0")

class CpgServerSwagger extends Swagger(Swagger.SpecVersion, "1.0.0", CpgServerApiInfo)
