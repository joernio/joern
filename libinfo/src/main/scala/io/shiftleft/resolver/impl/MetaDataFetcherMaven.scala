package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.Async
import cats.effect.kernel.Async
import cats.syntax.all.*
import io.shiftleft.resolver.api.{Coordinate, MetaData, MetaDataFetcher}
import io.shiftleft.resolver.util.{HttpUtil, PomContext, PomUtil}
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.xml.Elem

class MetaDataFetcherMaven[F[_]: Async: Parallel](httpClient: Client[F],
                                                  serverUrl: String)
  extends MetaDataFetcher[F, IdMaven] {
  private given Logger[F] = Slf4jLogger.getLogger[F]()

  private def uri(dep: Coordinate[IdMaven]): String = {
    val groupId = dep.id.groupId
    val artifactId = dep.id.artifactId
    val version = dep.version
    val groupPart = groupId.replace('.', '/')
    s"$serverUrl/$groupPart/$artifactId/$version/$artifactId-$version.pom"
  }

  override def fetch(deps: Vector[Coordinate[IdMaven]]): F[(Vector[Coordinate[IdMaven]], Vector[MetaData[IdMaven]])] = {
    val pairsF =
      deps.map { dep =>
        fetchAsMetaData(dep).map(metaData => (dep, Some(metaData)))
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

  private def fetchAsMetaData(dep: Coordinate[IdMaven]): F[MetaData[IdMaven]] = {
    HttpUtil.fetchAsString(httpClient, uri(dep)).flatMap { pomStr =>
      val xml = PomUtil.loadXml(pomStr)
      getContext(xml).map { context =>
        PomUtil.extractContent(xml, context)
      }
    }
  }

  private def getContext(xml: Elem): F[PomContext] = {
    PomUtil.extractParent(xml) match {
      case Some(parent) =>
        for {
          pomStr <- HttpUtil.fetchAsString(httpClient, uri(parent))
          xml = PomUtil.loadXml(pomStr)
          parentContext <- getContext(xml)
          context = PomUtil.extractContext(xml, parentContext)
          mergedContext = parentContext.merge(context)
        } yield mergedContext
      case None =>
        Async[F].pure(PomUtil.extractContext(xml, PomContext.empty))
    }
  }
}
