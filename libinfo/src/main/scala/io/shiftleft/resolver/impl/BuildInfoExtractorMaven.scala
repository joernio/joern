package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.*
import cats.syntax.all.*
import io.shiftleft.resolver.api.{BuildInfoExtractor, BuildTarget, Coordinate, MetaData, MetaDataCalculator, MetaDataFetcher, ResolutionModel}
import io.shiftleft.resolver.util.{PomContext, PomUtil}

import java.nio.file.{Files, Path}

class BuildInfoExtractorMaven[F[_]: Sync: Parallel] extends BuildInfoExtractor[F, IdMaven] {

  override def fileRelevant(path: Path): Boolean = {
    path.endsWith("pom.xml")
  }

  private def readPomContent(buildFile: Path): F[MetaData[IdMaven]] = {
    Sync[F].blocking(Files.readString(buildFile)).map { pomStr =>
      val xml = PomUtil.loadXml(pomStr)
      // TODO This does not work if the build files uses information from the parent context.
      //      Use proper context.
      PomUtil.extractContent(xml, PomContext.empty)
    }
  }

  private def pomContentsToBuildTargets(pomContents: Vector[MetaData[IdMaven]]): Vector[BuildTarget[IdMaven]] = {
    for {
      pomContent <- pomContents
      groupId = pomContent.coordinate.id.groupId
      artifactId = pomContent.coordinate.id.artifactId
    } yield BuildTarget(IdMaven(groupId, artifactId), pomContent.directDeps)
  }

  override def extractBuildTargets(buildFiles: List[Path]): F[Vector[BuildTarget[IdMaven]]] = {
    val separatePomContents = buildFiles.toVector.map(readPomContent)
    separatePomContents.parSequence.map(pomContentsToBuildTargets)
  }

}
