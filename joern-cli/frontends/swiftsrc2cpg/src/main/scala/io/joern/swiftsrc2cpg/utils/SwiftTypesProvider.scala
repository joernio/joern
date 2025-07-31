package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import io.joern.x2cpg.utils.Environment
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.io.StringReader
import java.nio.file.Paths
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex
import scala.util.{Try, Using}

/** Provides Swift type information by extracting type data from Swift compiler output. This provider uses the Swift
  * compiler's AST dump functionality to collect type information which is then used for populating the CPG with
  * accurate type references.
  */
object SwiftTypesProvider {

  private val logger = LoggerFactory.getLogger(getClass)

  private val MinimumSwiftVersion = "6.1" // This brings in the -dump-ast-format json option

  private val SwiftVersionCommand  = Seq("swift", "--version")
  private val SwiftcVersionCommand = Seq("swiftc", "--version")
  private val SwiftVersionCommands = Seq(SwiftVersionCommand, SwiftcVersionCommand)
  private val SwiftBuildCommand    = Seq("swift", "build", "--verbose")
  private val SwiftcDumpOptions    = Seq("-dump-ast", "-dump-ast-format", "json")

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
    * @param range
    *   The position range in the source file
    * @param tpe
    *   The fully qualified type name (if any; demangled)
    * @param fullName
    *   The fully qualified name (if any; demangled)
    * @param nodeKind
    *   The AST node kind
    */
  case class ResolvedTypeInfo(range: (Int, Int), tpe: Option[String], fullName: Option[String], nodeKind: String)

  type SwiftTypeMapping        = immutable.HashMap[String, immutable.List[ResolvedTypeInfo]]
  type MutableSwiftTypeMapping = mutable.HashMap[String, mutable.ListBuffer[ResolvedTypeInfo]]

  object TypeMappingConversion:
    given toImmutable: Conversion[MutableSwiftTypeMapping, SwiftTypeMapping] = mutableSwiftTypeMapping =>
      immutable.HashMap.from(mutableSwiftTypeMapping.map { case (filename, list) =>
        filename -> immutable.List.from(list)
      })

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
  private val SwiftcArgSplit = "(?<!\\\\) "

  private val SwiftcIgnoredArgs =
    Seq("-v", "-incremental", "-whole-module-optimization", "-parseable-output", "-serialize-diagnostics")

  /** Attempts to create a SwiftTypesProvider using the given configuration.
    *
    * @param config
    *   The Swift CPG generation configuration
    * @return
    *   Some(SwiftTypesProvider) if Swift environment is valid, None otherwise
    */
  def apply(config: Config): Option[SwiftTypesProvider] = {
    if (!config.swiftBuild) {
      logger.warn(
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

    config.xcodeOutputPath.map(outputPath => build(config, IOUtils.readLinesInFile(outputPath))).orElse(build(config))
  }

  private def resolveSwiftDemangleCommand(config: Config, args: Seq[String]): Seq[String] = {
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

  private def swiftDemangleVersionCommand(config: Config): Seq[String] = {
    resolveSwiftDemangleCommand(config, Seq("--version"))
  }

  private def swiftDemangleCommand(config: Config): Seq[String] = {
    resolveSwiftDemangleCommand(config, Seq("--compact", "--no-sugar"))
  }

  private def isValidEnvironment(config: Config): Boolean = {
    if (config.xcodeOutputPath.isEmpty && !Paths.get(config.inputPath, "Package.swift").toFile.canRead) {
      return false
    }

    val commands = if (config.xcodeOutputPath.isDefined) {
      // we do not need 'swift' on the system if the commands are taken from Xcode
      Seq(SwiftcVersionCommand)
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
    val hasSwiftDemangle = ExternalCommand.run(swiftDemangleVersionCommand(config)).successful
    hasSwift && hasSwiftDemangle
  }

  private def build(config: Config): Option[SwiftTypesProvider] = {
    ExternalCommand
      .run(SwiftBuildCommand, mergeStdErrInStdOut = true, workingDir = Some(Paths.get(config.inputPath)))
      .successOption
      .map(outLines => build(config, outLines))
  }

  private def argShouldBeFiltered(arg: String): Boolean = {
    SwiftcIgnoredArgs.contains(arg) || arg.startsWith("-emit") || arg == "-output-file-map"
  }

  private def parseSwiftcArgs(args: List[String]): List[String] = {
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

  private def isSwiftcInvocation(line: String): Boolean = {
    (line.contains("\\swiftc.exe ") || line.contains("/swiftc "))
    && line.contains(" -module-name ")
    && !line.contains(" -emit-executable ")
  }

  private def argsFromLine(line: String): List[String] = {
    line.replace("\\ -", " -").replace("/ -", " -").split(SwiftcArgSplit).toList
  }

  def build(config: Config, compilerOutput: Seq[String]): SwiftTypesProvider = {
    val swiftcInvocations       = compilerOutput.filter(isSwiftcInvocation)
    val parsedSwiftcInvocations = swiftcInvocations.map(l => parseSwiftcArgs(argsFromLine(l)) ++ SwiftcDumpOptions)
    new SwiftTypesProvider(config, parsedSwiftcInvocations)
  }
}

/** Creates and retrieves type mappings from Swift source code.
  *
  * @param config
  *   The configuration for the Swift CPG generation
  * @param parsedSwiftcInvocations
  *   The parsed Swift compiler invocations to extract type information
  */
case class SwiftTypesProvider(config: Config, parsedSwiftcInvocations: Seq[Seq[String]]) {

  import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.*

  import scala.language.implicitConversions
  import TypeMappingConversion.toImmutable

  private lazy val swiftDemangleCommand = SwiftTypesProvider.swiftDemangleCommand(config)

  private val mangledNameCache = mutable.HashMap.empty[String, Option[String]]

  private def demangle(mangledName: String): Option[String] = {
    if (mangledName.isEmpty) return None
    val strippedMangledName = mangledName.stripPrefix("$").replaceFirst(":", "")
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

  private def calculateFullname(mangledName: String): Option[String] = {
    mangledNameCache.getOrElseUpdate(
      mangledName,
      demangle(mangledName)
        .map {
          case MemberNameRegex(parent, name, rest) => removeModifier(s"$parent$name$rest")
          case other                               => removeModifier(other)
        }
        .map(AstCreatorHelper.stripGenerics)
        .map(_.replace(" ", ""))
    )
  }

  private def resolve(typeInfo: TypeInfo): ResolvedTypeInfo = {
    val demangledTpe      = typeInfo.tpe.flatMap(calculateTypename)
    val demangledFullName = typeInfo.fullName.flatMap(calculateFullname)
    ResolvedTypeInfo(typeInfo.range, demangledTpe, demangledFullName, typeInfo.nodeKind)
  }

  def mappingFromJson(jsonString: String, result: MutableSwiftTypeMapping): Unit = {
    Using(new StringReader(jsonString)) { reader =>
      GsonTypeInfoReader.collectTypeInfo(reader).foreach { typeInfo =>
        result.getOrElseUpdate(typeInfo.filename, mutable.ListBuffer.empty).addOne(resolve(typeInfo))
      }
    }
  }

  def retrieveMappings(): SwiftTypeMapping = {
    val mapping = new MutableSwiftTypeMapping
    parsedSwiftcInvocations.foreach { invocationCommand =>
      ExternalCommand
        .run(invocationCommand, mergeStdErrInStdOut = true, workingDir = Some(Paths.get(config.inputPath)))
        .successOption
        .foreach { outlines =>
          val jsonString = outlines.mkString
          mappingFromJson(jsonString, mapping)
        }
    }
    mapping
  }

}
