package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.Async
import cats.syntax.all.*
import io.shiftleft.resolver.api.{Coordinate, MetaData}
import io.shiftleft.resolver.util.{PomContext, PomUtil}
import org.http4s.client.Client

import scala.xml.Elem

class MetaDataFetcherMaven[F[_]: Async: Parallel](httpClient: Client[F],
                                                    serverUrl: String)
  extends MetaDataFetcherHttp[F, IdMaven](httpClient) {
  
  protected def uri(dep: Coordinate[IdMaven]): String = {
    val groupId = dep.id.groupId
    val artifactId = dep.id.artifactId
    val version = dep.version
    val groupPart = groupId.replace('.', '/')
    s"$serverUrl/$groupPart/$artifactId/$version/$artifactId-$version.pom"
  }

  override protected def fetchExtensionPoint(dep: Coordinate[IdMaven]): F[MetaData[IdMaven]] = {
    fetchSingle(dep).flatMap { pomStr =>
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
          pomStr <- fetchSingle(parent)
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
