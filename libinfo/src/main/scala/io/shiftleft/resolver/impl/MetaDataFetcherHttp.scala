package io.shiftleft.resolver.impl

import cats.{ApplicativeError, MonadError, Parallel}
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.text
import io.shiftleft.resolver.api.{Coordinate, Id, MetaData, MetaDataFetcher}
import org.http4s.client.Client
import org.http4s.{Method, Request, Status, Uri}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

abstract class MetaDataFetcherHttp[F[_]: Async: Parallel, I <: Id](httpClient: Client[F])
  extends MetaDataFetcher[F, I] {
  private given Logger[F] = Slf4jLogger.getLogger[F]()

  protected def uri(dep: Coordinate[I]): String

  override def fetch(deps: Vector[Coordinate[I]]): F[(Vector[Coordinate[I]], Vector[MetaData[I]])] = {
    val pairsF =
      deps.map { dep =>
        fetchExtensionPoint(dep).map(metaData => (dep, Some(metaData)))
          .handleErrorWith { throwable =>
            Logger[F].debug(throwable)(s"Error while fetching $dep") >>
              Async[F].pure((dep, None))
          }
      }.parSequence

    pairsF.map { pairs =>
      val (failedFetch, successfulFetch) = pairs.partition(_._2.isEmpty)
      (
        failedFetch.map(_._1),
        successfulFetch.map(_._2.get)
      )
    }
  }
  
  protected def fetchExtensionPoint(dep: Coordinate[I]): F[MetaData[I]]

  protected def fetchSingle(dep: Coordinate[I], retries: Int = 3): F[String] = {
    val request = Request[F](
      method = Method.GET,
      uri = Uri.fromString(uri(dep)).toOption.get
    )

    Logger[F].debug(s"HTTP $request for $dep") >>
      httpClient.run(request).attempt.use {
        case Right(response) =>
          Logger[F].debug(s"HTTP $response") >>
            (response.status match {
              case Status.Ok =>
                val responseBody = response.body.through(text.utf8.decode).compile.string
                responseBody
              case status =>
                throw new RuntimeException(s"Unexpected return status $status for $request")
            })
        case Left(throwable) =>
          Logger[F].info(s"HTTP exception ${throwable.getMessage}") >>
            (if (retries > 1) {
              fetchSingle(dep, retries - 1)
            } else {
              throw throwable
            })

      }

  }
}
