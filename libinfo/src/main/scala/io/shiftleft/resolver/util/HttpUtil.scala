package io.shiftleft.resolver.util

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits.catsSyntaxFlatMapOps
import fs2.text
import fs2.Stream
import org.http4s.{EntityBody, Method, Request, Status, Uri}
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

object HttpUtil {
  def fetchAsString[F[_]: Async: Logger](httpClient: Client[F],
                                         uri: String,
                                         retries: Int = 3,
                                        ): F[String] = {
    fetch(httpClient, uri, retries).use(_.through(text.utf8.decode).compile.string)
  }

  def fetch[F[_]: Async: Logger](httpClient: Client[F],
                                    uri: String,
                                    retries: Int = 3,
                                   ): Resource[F, Stream[F, Byte]] = {
    val request = Request[F](
      method = Method.GET,
      uri = Uri.fromString(uri).toOption.get
    )


    Resource.eval(Logger[F].debug(s"HTTP $request")) >>
      httpClient.run(request).map { response =>
        response.status match {
          case Status.Ok =>
            response.body
          case status =>
            throw new RuntimeException(s"Unexpected return status $status for $request")
        }
      }.handleErrorWith { (throwable: Throwable) =>
        Resource.eval(Logger[F].info(s"HTTP exception ${throwable.getMessage}")) >>
          (if (retries > 1) {
            fetch(httpClient, uri, retries - 1)
          } else {
            throw throwable
          })
      }
  }
}
