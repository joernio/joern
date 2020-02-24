package io.shiftleft.joern.server

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import io.shiftleft.cpgserver.config.ServerConfiguration
import io.shiftleft.cpgserver.query.ServerAmmoniteExecutor
import io.shiftleft.cpgserver.route.{CpgRoute, HttpErrorHandler, SwaggerRoute}
import io.shiftleft.joern.server.cpg.JoernCpgProvider
import io.shiftleft.joern.server.scripting.JoernServerAmmoniteExecutor

object JoernServer extends IOApp {

  private val banner: String =
    """ |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗    ███████╗███████╗██████╗ ██╗   ██╗███████╗██████╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║    ██╔════╝██╔════╝██╔══██╗██║   ██║██╔════╝██╔══██╗
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║    ███████╗█████╗  ██████╔╝██║   ██║█████╗  ██████╔╝
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║    ╚════██║██╔══╝  ██╔══██╗╚██╗ ██╔╝██╔══╝  ██╔══██╗
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║    ███████║███████╗██║  ██║ ╚████╔╝ ███████╗██║  ██║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝    ╚══════╝╚══════╝╚═╝  ╚═╝  ╚═══╝  ╚══════╝╚═╝  ╚═╝
        |""".stripMargin

  private val cpgProvider: JoernCpgProvider =
    new JoernCpgProvider

  private val ammoniteServerExecutor: ServerAmmoniteExecutor =
    new JoernServerAmmoniteExecutor

  private implicit val httpErrorHandler: HttpErrorHandler =
    CpgRoute.CpgHttpErrorHandler

  private val serverConfig: ServerConfiguration =
    ServerConfiguration.config.getOrElse(ServerConfiguration.default)

  private val httpRoutes: HttpRoutes[IO] =
    CpgRoute(cpgProvider, ammoniteServerExecutor, serverConfig.files).routes <+> SwaggerRoute().routes

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO]
      .withBanner(List(banner))
      .bindHttp(serverConfig.port, serverConfig.host)
      .withHttpApp(httpRoutes.orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
