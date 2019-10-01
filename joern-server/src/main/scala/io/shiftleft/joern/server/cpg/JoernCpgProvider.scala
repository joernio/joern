package io.shiftleft.joern.server.cpg

import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConverters._

import cats.data.OptionT
import cats.effect.IO
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.cpgserver.cpg.CpgProvider
import io.shiftleft.cpgserver.model.{CpgOperationFailure, CpgOperationResult, CpgOperationSuccess}
import io.shiftleft.joern.{CpgLoader, JoernParse}

class JoernCpgProvider extends CpgProvider {

  private val cpgMap = new ConcurrentHashMap[UUID, CpgOperationResult[Cpg]]().asScala

  private val uuidProvider = IO(UUID.randomUUID)

  private val tempFileProvider = IO(Files.createTempFile("", ".cpg.bin.zip"))

  private def constructCpg(cpgId: UUID, cpgFile: Path, fileNames: Array[String]): IO[Unit] = {
    IO(JoernParse.parse(fileNames, cpgFile.toString, enhance = true, dataFlow = true, CpgLoader.defaultSemanticsFile)).runAsync {
      case Right(_) => populateCpg(cpgId, cpgFile)
      case Left(ex) => IO(cpgMap.put(cpgId, CpgOperationFailure(ex))).map(_ => ())
    }.toIO
  }

  private def populateCpg(cpgId: UUID, cpgFile: Path): IO[Unit] = {
    IO(CpgLoader.load(cpgFile.toString)).runAsync {
      case Right(cpg) => IO(cpgMap.put(cpgId, CpgOperationSuccess(cpg))).map(_ => ())
      case Left(ex)   => IO(cpgMap.put(cpgId, CpgOperationFailure(ex))).map(_ => ())
    }.toIO
  }

  override def createCpg(fileNames: Set[String]): IO[UUID] = {
    for {
      logger <- Slf4jLogger.create[IO]
      cpgId <- uuidProvider
      cpgFile <- tempFileProvider
      _ <- logger.info(s"Creating CPG [$cpgId] at path [$cpgFile].")
      _ <- constructCpg(cpgId, cpgFile, fileNames.toArray)
      _ <- logger.info(s"CPG [$cpgId] scheduled for creation...")
    } yield cpgId
  }

  override def retrieveCpg(cpgId: UUID): OptionT[IO, CpgOperationResult[Cpg]] =
    OptionT.fromOption(cpgMap.get(cpgId))
}
