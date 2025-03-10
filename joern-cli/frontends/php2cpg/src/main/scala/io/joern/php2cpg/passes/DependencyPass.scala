package io.joern.php2cpg.passes

import io.joern.php2cpg.parser.Domain.PhpOperators
import io.shiftleft.codepropertygraph.generated.nodes.{NewDependency, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import upickle.default.*

import java.nio.file.{Files, Path, Paths}
import scala.annotation.targetName
import scala.util.{Failure, Success, Try, Using}

/** Parses the `composer.json` file for all `require` dependencies.
  */
class DependencyPass(cpg: Cpg, composerPaths: List[String]) extends ForkJoinParallelCpgPass[Path](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[Path] = composerPaths.map(Paths.get(_)).toArray

  override def runOnPart(builder: DiffGraphBuilder, composerFile: Path): Unit = {
    val composer =
      Using.resource(Files.newInputStream(composerFile)) { is =>
        Try(read[Composer](ujson.Readable.fromByteArray(is.readAllBytes())))
      } match {
        case Failure(exception) =>
          logger.error(s"Unable to deserialize `${composerFile.toString}`", exception)
          Composer()
        case Success(composer) => composer
      }
    composer.require.foreach { case (name, version) =>
      builder.addNode(NewDependency().name(name).version(version))
    }
    // For `autoload`, the version is the matcher path prefixed with `autoload`
    composer.autoload.`psr-4`.foreach {
      case (namespace, PsrString(name)) =>
        builder.addNode(NewDependency().name(namespace).version(s"${PhpOperators.autoload}$name"))
      case (namespace, PsrArray(array)) =>
        array.foreach { name =>
          builder.addNode(NewDependency().name(namespace).version(s"${PhpOperators.autoload}$name"))
        }
    }
  }

}

sealed trait PsrStringOrArray {
  def obj: String | Array[String]
}

case class PsrString(obj: String) extends PsrStringOrArray

case class PsrArray(obj: Array[String]) extends PsrStringOrArray

object PsrStringOrArray:
  given ReadWriter[PsrStringOrArray] = {
    val logger = LoggerFactory.getLogger(getClass)

    readwriter[ujson.Value]
      .bimap[PsrStringOrArray](
        x =>
          x.obj match {
            case o: String        => ujson.Str(o)
            case o: Array[String] => ujson.Arr(o.map(ujson.Str.apply)*)
          },
        {
          case json @ (j: ujson.Str) => PsrString(json.str)
          case json @ (j: ujson.Arr) => PsrArray(json.arr.map(_.str).toArray)
          case x =>
            logger.warn(s"Unexpected value type for `autoload.psr-4`: ${x.getClass}")
            PsrString("<unknown>")
        }
      )
  }

case class Autoload(
  @targetName("psr0") `psr-0`: Map[String, PsrStringOrArray] = Map.empty,
  @targetName("psr4") `psr-4`: Map[String, PsrStringOrArray] = Map.empty
) derives ReadWriter

case class Composer(require: Map[String, String] = Map.empty, autoload: Autoload = Autoload()) derives ReadWriter
