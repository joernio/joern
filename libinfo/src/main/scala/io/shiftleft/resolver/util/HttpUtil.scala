package io.shiftleft.resolver.util

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
    fetch(httpClient, uri, retries, _.through(text.utf8.decode).compile.string)
  }

  def fetchAsBytes[F[_]: Async: Logger](httpClient: Client[F],
                                        uri: String,
                                        retries: Int = 3,
                                       ): F[Array[Byte ]] = {
    fetch(httpClient, uri, retries, _.compile.to(Array))
  }

  def fetch[F[_]: Async: Logger, R](httpClient: Client[F],
                                 uri: String,
                                 retries: Int,
                                 consumer: Stream[F, Byte] => F[R]
                 ): F[R] = {
    val request = Request[F](
      method = Method.GET,
      uri = Uri.fromString(uri).toOption.get
    )

    Logger[F].debug(s"HTTP $request") >>
    httpClient.run(request).attempt.use {
      case Right(response) =>
        Logger[F].debug(s"HTTP $response") >>
          (response.status match {
            case Status.Ok =>
              consumer(response.body)
            case status =>
              throw new RuntimeException(s"Unexpected return status $status for $request")
          })
      case Left(throwable) =>
        Logger[F].info(s"HTTP exception ${throwable.getMessage}") >>
          (if (retries > 1) {
            fetch(httpClient, uri, retries - 1, consumer)
          } else {
            throw throwable
          })
    }
  }
}
