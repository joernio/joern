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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
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

  private def fetchAsLibInfoHandle(dep: Coordinate[IdMaven]): F[LibInfoHandle] = {
    HttpUtil.fetch(httpClient, uri(dep)).use { bodyStream =>
      bodyStream.through(fs2.io.toInputStream).evalMap { inputStream =>
        createLibInfoHandle(inputStream)
      }.compile.onlyOrError
    }
  }

  private def createLibInfoHandle(inputStream: InputStream): F[LibInfoHandle] = {
    Async[F].blocking {
      val outputStream = new ByteArrayOutputStream()
      Using.resource(new LibInfoWriter(outputStream)) { libInfoWriter =>
        val jarInputStream = new JarInputStream(inputStream)
        var jarEntry: JarEntry = null
        while ( {
          jarEntry = jarInputStream.getNextJarEntry;
          jarEntry != null
        }) {
          if (jarEntry.getName.endsWith(".class")) {
            new LibInfoGenJvm(libInfoWriter).convertInputStream(jarInputStream)
          }
        }
        new LibInfoHandle(outputStream.toString)
      }
    }
  }

  override def fetch2(deps: fs2.Stream[F, Coordinate[IdMaven]]
                     ): fs2.Stream[F, (Coordinate[IdMaven], Option[LibInfoHandle])] = {
    deps.evalMap { dep =>
      fetchAsLibInfoHandle(dep).map(Some.apply).handleErrorWith { throwable =>
        Logger[F].debug(throwable)(s"Error while fetching $dep") >>
          Async[F].pure(None)
      }.map((dep, _))
    }
  }
}
