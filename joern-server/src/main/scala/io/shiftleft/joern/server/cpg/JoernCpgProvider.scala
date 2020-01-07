package io.shiftleft.joern.server.cpg

import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.{ConcurrentHashMap, Executors}

import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext
import cats.data.OptionT
import cats.effect.{Blocker, ContextShift, IO}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.cpgserver.cpg.CpgProvider
import io.shiftleft.cpgserver.query.{CpgOperationFailure, CpgOperationResult, CpgOperationSuccess}
import io.shiftleft.joern.JoernParse.ParserConfig
import io.shiftleft.joern.{CpgLoader, JoernParse}

class JoernCpgProvider(fileExtensions: Set[String] = Set(".c", ".cc", ".cpp", ".h", ".hpp"))(implicit val cs: ContextShift[IO])
    extends CpgProvider {

  private val blocker: Blocker =
    Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2)))

  private val cpgMap = new ConcurrentHashMap[UUID, CpgOperationResult[Cpg]]().asScala

  private val uuidProvider = IO(UUID.randomUUID)

  private val tempFileProvider = IO(Files.createTempFile("", ".cpg.bin.zip"))

  private def constructCpg(cpgId: UUID,
                           cpgFile: Path,
                           fileNames: Set[String],
                           fileExtensions: Set[String]): IO[Unit] = {
    val parserConfig = ParserConfig(
      inputPaths = fileNames,
      sourceFileExtensions = fileExtensions,
      outputCpgFile = cpgFile.toString
    )

    blocker
      .blockOn(IO(JoernParse.generateCpg(parserConfig)))
      .runAsync {
        case Right(_) => populateCpg(cpgId, cpgFile)
        case Left(ex) => IO(cpgMap.put(cpgId, CpgOperationFailure(ex))).map(_ => ())
      }
      .toIO
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
      _ <- constructCpg(cpgId, cpgFile, fileNames, fileExtensions)
      _ <- logger.info(s"CPG [$cpgId] scheduled for creation...")
    } yield cpgId
  }

  override def retrieveCpg(cpgId: UUID): OptionT[IO, CpgOperationResult[Cpg]] =
    OptionT.fromOption(cpgMap.get(cpgId))
}
