package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.kernel.Async
import cats.syntax.all.*
import io.shiftleft.libinfo.LibInfoWriter
import io.shiftleft.libinfo.generator.jvm.LibInfoGenJvm
import io.shiftleft.resolver.api.{Coordinate, LibInfoFetcher, LibInfoHandle}
import io.shiftleft.resolver.util.HttpUtil
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.jar.{JarEntry, JarInputStream}
import scala.util.Using

class LibInfoFetcherMaven[F[_]: Async: Parallel](httpClient: Client[F],
                                                 serverUrl: String) extends LibInfoFetcher[F, IdMaven] {
  private given Logger[F] = Slf4jLogger.getLogger[F]()

  private def uri(dep: Coordinate[IdMaven]): String = {
    val groupId = dep.id.groupId
    val artifactId = dep.id.artifactId
    val version = dep.version
    val groupPart = groupId.replace('.', '/')
    s"$serverUrl/$groupPart/$artifactId/$version/$artifactId-$version.jar"
  }

  override def fetch(deps: Vector[Coordinate[IdMaven]]): F[(Vector[Coordinate[IdMaven]], Vector[LibInfoHandle])] = {
    val pairsF =
      deps.map { dep =>
        fetchAsLibInfoHandle(dep).map(libInfoHandle => (dep, Some(libInfoHandle)))
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
  
  private def fetchAsLibInfoHandle(dep: Coordinate[IdMaven]): F[LibInfoHandle] = {
    HttpUtil.fetchAsBytes(httpClient, uri(dep))
      .flatMap { byteArray =>
        val inputStream = new ByteArrayInputStream(byteArray)
        val outputStream = new ByteArrayOutputStream()
        val libInfoWriter = new LibInfoWriter(outputStream)
        Async[F].delay(new JarInputStream(inputStream)).map { jarInputStream =>
          var jarEntry: JarEntry = null
          while ({jarEntry = jarInputStream.getNextJarEntry; jarEntry != null}) {
            if (jarEntry.getName.endsWith(".class")) {
              new LibInfoGenJvm(libInfoWriter).convertInputStream(jarInputStream)
            }
          }
          libInfoWriter.close
          new LibInfoHandle(outputStream.toString)
        }
      }
  }
}
