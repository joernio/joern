package io.joern.php2cpg.passes

import better.files.File
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.shiftleft.codepropertygraph.generated.nodes.{NewDependency, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import upickle.default.*

import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

/** Parses the `composer.json` file for all `require` dependencies.
  */
class DependencyPass(cpg: Cpg, composerPaths: List[String]) extends ForkJoinParallelCpgPass[File](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[File] = composerPaths.map(File(_)).toArray

  override def runOnPart(builder: DiffGraphBuilder, composerFile: File): Unit = {
    val composer =
      composerFile.inputStream.apply(is => Try(read[Composer](ujson.Readable.fromByteArray(is.readAllBytes())))) match {
        case Failure(exception) =>
          logger.error("Unable to deserialize `composer.json`", exception)
          Composer()
        case Success(composer) => composer
      }
    composer.require.foreach { case (name, version) =>
      builder.addNode(NewDependency().name(name).version(version))
    }
    // For `autoload`, the version is the matcher path prefixed with `autoload`
    composer.autoload.`psr-4`.foreach {
      case (namespace, StringOrArray(name: String)) =>
        builder.addNode(NewDependency().name(namespace).version(s"${PhpOperators.autoload}$name"))
      case (namespace, StringOrArray(array: Array[String])) =>
        array.foreach { name =>
          builder.addNode(NewDependency().name(namespace).version(s"${PhpOperators.autoload}$name"))
        }
    }
  }

}

case class StringOrArray(obj: String | Array[String])

object StringOrArray:
  given ReadWriter[StringOrArray] = {
    val logger = LoggerFactory.getLogger(getClass)

    readwriter[ujson.Value]
      .bimap[StringOrArray](
        x =>
          x.obj match {
            case o: String        => ujson.Str(o)
            case o: Array[String] => ujson.Arr(o.map(ujson.Str.apply)*)
          },
        {
          case json @ (j: ujson.Str) => StringOrArray(json.str)
          case json @ (j: ujson.Arr) => StringOrArray(json.arr.map(_.str).toArray)
          case x =>
            logger.warn(s"Unexpected value type for `autoload.psr-4`: ${x.getClass}")
            StringOrArray("<unknown>")
        }
      )
  }

case class Autoload(@targetName("psr4") `psr-4`: Map[String, StringOrArray] = Map.empty) derives ReadWriter

case class Composer(require: Map[String, String] = Map.empty, autoload: Autoload = Autoload()) derives ReadWriter
