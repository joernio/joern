package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import io.joern.x2cpg.utils.Environment
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.shiftleft.semanticcpg.utils.FileUtil.PathExt
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.io.{BufferedReader, InputStream, InputStreamReader, StringReader}
import java.nio.file.{Files, Path, Paths}
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.collection.parallel.ParSeq
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex
import scala.util.{Failure, Try, Using}

/** Provides Swift type information by extracting type data from Swift compiler output. This provider uses the Swift
  * compiler's AST dump functionality to collect type information which is then used for populating the CPG with
  * accurate type references.
  */
object SwiftTypesProvider {

  private val logger = LoggerFactory.getLogger(getClass)

  private val MinimumSwiftVersion = "6.1" // This brings in the -dump-ast-format json option

  private val SwiftVersionCommand         = Seq("swift", "--version")
  private val SwiftCompilerVersionCommand = Seq("swiftc", "--version")
  private val SwiftVersionCommands        = Seq(SwiftVersionCommand, SwiftCompilerVersionCommand)
  private val SwiftBuildCommand           = Seq("swift", "build", "--verbose")
  private val SwiftCleanCommand           = Seq("swift", "package", "clean")
  private val SwiftCompilerDumpOptions    = Seq("-dump-ast", "-dump-ast-format", "json", "-suppress-warnings")

  /** Information about a Swift type found in the source code. fullName and tpe are not demangled yet.
    *
    * @param filename
    *   The source file where the type was found
    * @param range
    *   The position range in the source file
    * @param tpe
    *   The fully qualified type name (if any; not demangled)
    * @param fullName
    *   The fully qualified name (if any; not demangled)
    * @param nodeKind
    *   The AST node kind
    */
  case class TypeInfo(
    filename: String,
    range: (Int, Int),
    tpe: Option[String],
    fullName: Option[String],
    nodeKind: String
  ) {
    override def equals(obj: Any): Boolean = obj match {
      case that: TypeInfo =>
        this.filename == that.filename &&
        this.range == that.range &&
        this.nodeKind == that.nodeKind
      case _ => false
    }

    override def hashCode(): Int = {
      val prime  = 31
      var result = 1
      result = prime * result + filename.hashCode
      result = prime * result + range.hashCode
      prime * result + nodeKind.hashCode
    }
  }

  /** Information about a Swift type found in the source code. fullName and tpe are demangled.
    *
    * @param tpe
    *   The fully qualified type name (if any; demangled)
    * @param fullName
    *   The fully qualified name (if any; demangled)
    * @param nodeKind
    *   The AST node kind
    */
  case class ResolvedTypeInfo(tpe: Option[String], fullName: Option[String], nodeKind: String)

  type SwiftFileLocalTypeMapping = mutable.HashMap[(Int, Int), mutable.HashSet[ResolvedTypeInfo]]
  type SwiftTypeMapping          = TrieMap[String, SwiftFileLocalTypeMapping]

  /** @see
    *   [[io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.DeclModifierSyntax]]
    */
  private val SwiftDeclModifier = Set(
    "__consuming",
    "__setter_access",
    "_const",
    "_local",
    "actor",
    "async",
    "borrowing",
    "class",
    "consuming",
    "convenience",
    "distributed",
    "dynamic",
    "fileprivate",
    "final",
    "indirect",
    "infix",
    "internal",
    "isolated",
    "lazy",
    "mutating",
    "nonisolated",
    "nonmutating",
    "open",
    "optional",
    "override",
    "package",
    "postfix",
    "prefix",
    "private",
    "public",
    "reasync",
    "_resultDependsOnSelf",
    "required",
    "static",
    "transferring",
    "unowned",
    "weak"
  )

  /** The regular expression pattern `(?<!\\\\)` is used to split a string of spaces, but only if the space is not
    * preceded by a backslash.
    *
    *   - `(?<!\\\\)` is a negative lookbehind that checks if there is no backslash before the current position
    *   - ` ` is the actual space character after which it is split
    *
    * The pattern in the `argsFromLine` function is used to split command line arguments, preserving backslash escaped
    * spaces (`\ `) within an argument. This is important for arguments that contain spaces, such as file paths.
    *
    * Example: The string {{{file1.swift file\ with\ spaces.swift -o output}}} would be split into
    * {{{["file1.swift", "file\ with\ spaces.swift", "-o", "output"]}}}.
    */
  private val SwiftCompilerArgSplit = "(?<!\\\\) "

  private val SwiftCompilerIgnoredArgs =
    Seq(
      "-v",
      "-output-file-map",
      "-incremental",
      "-whole-module-optimization",
      "-parseable-output",
      "-serialize-diagnostics",
      "-Wwarning",
      "-warnings-as-errors"
    )

  /** Attempts to create a SwiftTypesProvider using the given configuration.
    *
    * @param config
    *   The Swift CPG generation configuration
    * @return
    *   Some(SwiftTypesProvider) if Swift environment is valid, None otherwise
    */
  def apply(config: Config): Option[SwiftTypesProvider] = {
    if (!config.swiftBuild) {
      logger.debug(
        "Swift build disabled (--swift-build was not provided). CPG will not contain any type information from the Swift compiler."
      )
      return None
    }
    if (!isValidEnvironment(config)) {
      logger.warn(
        "No valid Swift environment found (swift and swiftc executables). CPG will not contain any type information from the Swift compiler."
      )
      return None
    }

    config.buildLogPath.map(outputPath => build(config, IOUtils.readLinesInFile(outputPath))).orElse(build(config))
  }

  private def resolveSwiftDemangleCommand(args: Seq[String]): Seq[String] = {
    val defaultCommand = "swift-demangle" +: args
    if (Environment.operatingSystem == Environment.OperatingSystemType.Windows) {
      // Windows installation of swift puts swift-demangle into the PATH automatically
      defaultCommand
    } else {
      // Installer for Linux and macOS do not
      ExternalCommand
        .run(Seq("which", "swiftc"))
        .successOption
        .map { outLines =>
          val resolvedPath = Paths.get(outLines.mkString).toRealPath().resolveSibling("swift-demangle").toString
          resolvedPath +: args
        }
        .getOrElse(defaultCommand)
    }
  }

  private def swiftDemangleVersionCommand(): Seq[String] = {
    resolveSwiftDemangleCommand(Seq("--version"))
  }

  private def swiftDemangleCommand(): Seq[String] = {
    resolveSwiftDemangleCommand(Seq("--compact", "--no-sugar"))
  }

  private def isValidEnvironment(config: Config): Boolean = {
    if (config.buildLogPath.isEmpty && !Paths.get(config.inputPath, "Package.swift").toFile.canRead) {
      logger.debug("Package.swift not found or can not be read. This does not look like a valid SwiftPM project.")
      return false
    }

    val commands = if (config.buildLogPath.isDefined) {
      // we do not need 'swift' on the system if the commands are taken from Xcode
      Seq(SwiftCompilerVersionCommand)
    } else {
      SwiftVersionCommands
    }
    val hasSwift = commands.forall { command =>
      ExternalCommand.run(command).successOption match {
        case Some(outLines) =>
          val swiftVersion = outLines.find(_.startsWith("Swift version ")).map { str =>
            str.substring("Swift version ".length, str.indexOf(" ("))
          }
          swiftVersion.exists { v =>
            val isCompatible = Try(VersionHelper.compare(v, MinimumSwiftVersion)).toOption.getOrElse(-1) >= 0
            if (!isCompatible) { logger.debug(s"Found Swift version '$v' but '$MinimumSwiftVersion' is required!") }
            isCompatible
          }
        case _ =>
          logger.debug("No Swift version on this system found!")
          false
      }
    }
    val hasSwiftDemangle = ExternalCommand.run(swiftDemangleVersionCommand()).successful
    hasSwift && hasSwiftDemangle
  }

  private def build(config: Config): Option[SwiftTypesProvider] = {
    logger.info("Building Swift type map from SwiftPM project configuration")

    logger.debug("Cleaning the project first ...")
    ExternalCommand.run(SwiftCleanCommand, workingDir = Some(Paths.get(config.inputPath))).logIfFailed()

    logger.debug("Building the project ...")
    ExternalCommand
      .run(SwiftBuildCommand, mergeStdErrInStdOut = true, workingDir = Some(Paths.get(config.inputPath)))
      .logIfFailed()
      .successOption
      .map(outLines => build(config, outLines))
  }

  private def argShouldBeFiltered(arg: String): Boolean = {
    SwiftCompilerIgnoredArgs.contains(arg) || arg.startsWith("-emit")
  }

  private def parseSwiftCompilerArgs(args: List[String]): List[String] = {
    @scala.annotation.tailrec
    def loop(remaining: List[String], result: List[String]): List[String] = {
      remaining match {
        case Nil => result.reverse
        case head :: tail if argShouldBeFiltered(head) =>
          val remainingArgs = if (tail.headOption.exists(!_.startsWith("-"))) {
            val nextFlagIdx = tail.indexWhere(_.startsWith("-"))
            if (nextFlagIdx >= 0) tail.drop(nextFlagIdx) else Nil
          } else { tail }
          loop(remainingArgs, result)
        case head :: tail => loop(tail, head :: result)
      }
    }
    loop(args, Nil)
  }

  private def isSwiftInvocation(line: String): Boolean = {
    (line.contains("\\swiftc.exe ") || line.contains("/swiftc "))
    && line.contains(" -module-name ")
    && !line.contains(" -emit-executable ")
  }

  private def argsFromLine(line: String): List[String] = {
    line.replace("\\ -", " -").replace("/ -", " -").split(SwiftCompilerArgSplit).toList
  }

  /** Lists all readable directories under the given path.
    *
    * @param path
    *   The starting directory path to search from
    * @return
    *   A set of names of the readable directories found under path
    */
  private def listReadableDirectories(path: Path): Set[String] = {
    if (!Files.isDirectory(path) || !Files.isReadable(path)) {
      return Set.empty
    }
    try {
      path
        .listFiles()
        .filter(p => Files.isDirectory(p) && Files.isReadable(p))
        .flatMap(_.nameOption)
        .toSet
    } catch {
      case e: Exception =>
        logger.warn(s"Error listing module directories under ${path.toFile.toString}: ${e.getMessage}")
        Set.empty
    }
  }

  def build(config: Config, compilerOutput: Seq[String]): SwiftTypesProvider = {
    config.buildLogPath.foreach(path =>
      logger.info(s"Building Swift type map from compiler log at ${path.toFile.toString}")
    )

    val sourceModules    = listReadableDirectories(Paths.get(config.inputPath, "Sources"))
    val swiftInvocations = compilerOutput.filter(isSwiftInvocation)
    val parsedSwiftInvocations =
      swiftInvocations.map(l => parseSwiftCompilerArgs(argsFromLine(l)) ++ SwiftCompilerDumpOptions).filter { args =>
        val moduleNameIndex = args.indexOf("-module-name")
        val moduleName      = args(moduleNameIndex + 1)
        sourceModules.isEmpty || sourceModules.contains(moduleName)
      }
    new SwiftTypesProvider(config, parsedSwiftInvocations.distinct)
  }
}

/** Creates and retrieves type mappings from Swift source code.
  *
  * @param config
  *   The configuration for the Swift CPG generation
  * @param parsedSwiftInvocations
  *   The parsed Swift compiler invocations to extract type information
  */
case class SwiftTypesProvider(config: Config, parsedSwiftInvocations: Seq[Seq[String]]) {

  import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.*

  private lazy val swiftDemangleCommand = SwiftTypesProvider.swiftDemangleCommand()

  private val mangledNameCache = TrieMap.empty[String, Option[String]]

  private def demangle(mangledName: String): Option[String] = {
    if (mangledName.isEmpty) return None
    val strippedMangledName = mangledName.stripPrefix("$").replaceFirst("s:e:s:", "s:").replace(":", "")
    ExternalCommand
      .run(swiftDemangleCommand :+ strippedMangledName)
      .successOption
      .map(outLines => outLines.mkString.trim)
  }

  private def removeModifier(fullName: String): String = {
    SwiftDeclModifier.foldLeft(fullName) { (cur, repl) =>
      if (cur.startsWith(s"$repl ") || cur.contains(s" $repl ")) {
        cur.replace(s" $repl ", " ").stripPrefix(s"$repl ")
      } else cur
    }
  }

  private def calculateTypename(mangledName: String): Option[String] = {
    mangledNameCache.getOrElseUpdate(
      mangledName,
      demangle(mangledName).map(removeModifier).map(AstCreatorHelper.stripGenerics).map(_.replace(" ", ""))
    )
  }

  /** This regex is designed to clean up Swift demangled type names by extracting meaningful parts and removing internal
    * representation details. It has three capture groups:
    *
    *   1. `([^(]+)` - Captures everything before the first opening parenthesis, Example:
    *      `SwiftHelloWorldLib.HelloWorld.`
    *
    *   1. `\((.+)\sin\s_[^)]+\)` - Matches a pattern with internal identifier details:
    *      - `\(` - Literal opening parenthesis
    *      - `(.+)` - Captures the name inside parentheses (before "in _...")
    *      - `\sin\s_[^)]+\)` - Matches " in _" followed by an identifier and closing parenthesis
    *   1. `(.*)` - Captures everything after the closing parenthesis. Example: `.getter : Swift.String`
    *
    * Full example: `SwiftHelloWorldLib.HelloWorld.(suffix in _C6D5E4A96804CD03B7512662F178D1D8).getter : Swift.String`
    * becomes: `SwiftHelloWorldLib.HelloWorld.suffix.getter : Swift.String`
    */
  private val MemberNameRegex: Regex = """([^(]+)\((.+)\sin\s_[^)]+\)(.*)""".r

  /** Basically the same as with MemberNameRegex. But here for extension function fullnames.
    *
    * E.g., `(extension in FooExt)Foo.bar() -> Swift.String` becomes `FooExt.Foo.bar() -> Swift.String`.
    */
  private val ExtensionNameRegex: Regex = """\(extension\sin\s([^)]+)\):(.*)""".r

  private def calculateFullname(mangledName: String): Option[String] = {
    mangledNameCache.getOrElseUpdate(
      mangledName,
      demangle(mangledName)
        .map {
          case MemberNameRegex(parent, name, rest) => removeModifier(s"$parent$name$rest")
          case ExtensionNameRegex(name, rest)      => removeModifier(s"$name$rest")
          case other                               => removeModifier(other)
        }
        .map(AstCreatorHelper.stripGenerics)
        .map(_.replace(" ", ""))
    )
  }

  private def resolve(typeInfo: TypeInfo): ResolvedTypeInfo = {
    val demangledTpe      = typeInfo.tpe.flatMap(calculateTypename)
    val demangledFullName = typeInfo.fullName.flatMap(calculateFullname)
    ResolvedTypeInfo(demangledTpe, demangledFullName, typeInfo.nodeKind)
  }

  def mappingFromJson(jsonString: String, result: SwiftTypeMapping): Unit = {
    Using.resource(new StringReader(jsonString)) { reader =>
      GsonTypeInfoReader.collectTypeInfo(reader).foreach { typeInfo =>
        val range    = typeInfo.range
        val filename = typeInfo.filename
        result
          .getOrElseUpdate(
            filename, {
              logger.info(s"Generating type map for: $filename")
              mutable.HashMap.empty
            }
          )
          .getOrElseUpdate(range, mutable.HashSet.empty)
          .addOne(resolve(typeInfo))
      }
    }
  }

  private def stdOutFromSwiftCompiler(invocationCommand: Seq[String]): InputStream = {
    val builder = new ProcessBuilder().command(invocationCommand.toArray*)
    builder.directory(Paths.get(config.inputPath).toFile)
    builder.redirectErrorStream(true)
    builder.start().getInputStream
  }

  private def parSeqFrom(reader: BufferedReader): ParSeq[String] = {
    Iterator
      .continually(reader.readLine())
      .takeWhile(_ != null)
      .filter(_.startsWith("{"))
      .toSeq
      .par
  }

  def retrieveMappings(): SwiftTypeMapping = {
    val mapping = new SwiftTypeMapping
    parsedSwiftInvocations.par.foreach { invocationCommand =>
      Using.Manager { use =>
        val reader = use(new InputStreamReader(stdOutFromSwiftCompiler(invocationCommand)))
        val stdOut = use(new BufferedReader(reader))
        parSeqFrom(stdOut).foreach(jsonString => mappingFromJson(jsonString, mapping))
      } match {
        case Failure(exception) =>
          // Using.Manager swallows exceptions otherwise
          logger.warn("Error during Swift type map creation", exception)
        case _ => // this is fine
      }
    }
    logger.info(s"Got ${mapping.size} type map entries.")
    mapping
  }

}
