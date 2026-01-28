package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import io.joern.swiftsrc2cpg.utils.ExternalCommand.*
import io.joern.x2cpg.utils.Environment
import io.shiftleft.semanticcpg.utils.FileUtil.PathExt
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.io.{BufferedReader, InputStreamReader, StringReader}
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.collection.parallel.ExecutionContextTaskSupport
import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
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
  private val SwiftDemangleCommand        = Seq("swift-demangle")
  private val WhichSwiftCompilerCommand   = Seq("which", "swiftc")
  private val XcrunFindDemangleCommand    = Seq("xcrun", "--find") ++ SwiftDemangleCommand
  private val SwiftCompilerDumpOptions    = Seq("-suppress-warnings", "-dump-ast", "-dump-ast-format", "json")

  /** Information about a Swift type found in the source code. fullName and tpe are not demangled yet.
    *
    * @param filename
    *   The source file where the type was found
    * @param range
    *   The position range in the source file
    * @param typeFullname
    *   The fully qualified type name (if any; not demangled)
    * @param declFullname
    *   The fully qualified decl name (if any; not demangled)
    * @param nodeKind
    *   The AST node kind
    */
  case class TypeInfo(
    filename: String,
    range: (Int, Int),
    typeFullname: Option[String],
    declFullname: Option[String],
    inherits: Seq[String],
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

  /** Information about a Swift type found in the source code. The provided fullNames are demangled.
    *
    * @param typeFullname
    *   The fully qualified type name (if any; demangled)
    * @param declFullname
    *   The fully qualified declaration name (if any; demangled)
    * @param inherits
    *   A sequence of fully qualified inherited type names (if any; demangled)
    * @param nodeKind
    *   The AST node kind where this information was observed
    */
  case class ResolvedTypeInfo(
    typeFullname: Option[String],
    declFullname: Option[String],
    inherits: Seq[String],
    nodeKind: String
  )

  /**   - [[java.util.concurrent.ConcurrentHashMap]] provides atomic compute and computeIfAbsent operations used here to
    *     create-or-update entries and mutate the inner HashSet safely per key, without extra locks or retry loops.
    *   - TrieMap lacks equivalent APIs with the same per-key atomicity semantics; achieving the same would require
    *     CAS/retries or external synchronization, hurting clarity and throughput.
    *   - ConcurrentHashMap is highly optimized and scales better under contention; in practice it outperforms
    *     [[scala.collection.concurrent.TrieMap]] for hot, fine-grained updates like we do in
    *     [[SwiftTypesProvider.mappingFromJson]].
    *   - Keeps interop consistent with the rest of the JDK concurrency used here (executors, I/O), and plays well with
    *     `.asScala` when materializing immutable views.
    */
  private type MutableSwiftFileLocalTypeMapping =
    java.util.concurrent.ConcurrentHashMap[(Int, Int), mutable.HashSet[ResolvedTypeInfo]]
  type MutableSwiftTypeMapping =
    java.util.concurrent.ConcurrentHashMap[String, MutableSwiftFileLocalTypeMapping]

  type SwiftFileLocalTypeMapping = Map[(Int, Int), Set[ResolvedTypeInfo]]
  type SwiftTypeMapping          = Map[String, SwiftFileLocalTypeMapping]

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
      "-experimental-emit-module-separately",
      "-explicit-module-build",
      "-output-file-map",
      "-incremental",
      "-whole-module-optimization",
      "-parseable-output",
      "-use-frontend-parseable-output",
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

  /** Resolves the path to the swift-demangle command and combines it with the provided arguments. This method
    * determines the appropriate command to use for swift-demangle based on the operating system.
    *
    * @param args
    *   Arguments to pass to the swift-demangle command
    * @return
    *   A sequence containing the resolved swift-demangle command path followed by the provided arguments
    */
  private def resolveSwiftDemangleCommand(args: Seq[String]): Seq[String] = {
    val defaultCommand = SwiftDemangleCommand ++ args
    val osSpecificCommand = Environment.operatingSystem match {
      case io.joern.x2cpg.utils.Environment.OperatingSystemType.Windows =>
        // The Windows installation of Swift puts swift-demangle into the PATH automatically
        Some(defaultCommand)
      case io.joern.x2cpg.utils.Environment.OperatingSystemType.Linux =>
        // The Linux installation of Swift puts swift-demangle next to swiftc but not into the PATH automatically
        findInStdOut(WhichSwiftCompilerCommand)
          .map { outLine =>
            val resolvedPath = Paths.get(outLine).toRealPath().resolveSibling("swift-demangle").toString
            resolvedPath +: args
          }
      case io.joern.x2cpg.utils.Environment.OperatingSystemType.Mac =>
        // On macOS, we have to ask xcrun --find swift-demangle
        findInStdOut(XcrunFindDemangleCommand).map { outLine => outLine +: args }
      case io.joern.x2cpg.utils.Environment.OperatingSystemType.Unknown =>
        Some(defaultCommand)
    }
    osSpecificCommand.getOrElse(defaultCommand)
  }

  /** Returns the command to get the version of swift-demangle.
    *
    * This method constructs a command sequence that includes the path to the swift-demangle executable (resolved using
    * `resolveSwiftDemangleCommand`) along with the --version flag.
    *
    * @return
    *   A sequence containing the swift-demangle command with the --version argument
    */
  private def swiftDemangleVersionCommand(): Seq[String] = {
    resolveSwiftDemangleCommand(Seq("--version"))
  }

  /** Returns the command to demangle Swift symbols with specific formatting options.
    *
    * This method constructs a command sequence that includes the path to the swift-demangle executable (resolved using
    * `resolveSwiftDemangleCommand`) along with the following options:
    *   - `--compact`: Produces more compact demangled output
    *   - `--no-sugar`: Avoids syntactic sugar in the demangled output, showing raw type information
    *
    * @return
    *   A sequence containing the swift-demangle command with formatting options
    */
  private def swiftDemangleCommand(): Seq[String] = {
    resolveSwiftDemangleCommand(Seq("--compact", "--no-sugar"))
  }

  /** Checks if the current environment has all the required tools to perform Swift type extraction.
    *
    * This method verifies:
    *   - If Package.swift exists (when buildLogPath is not provided)
    *   - If Swift/swiftc are available with version >= 6.1
    *   - If swift-demangle tool is available
    *
    * @param config
    *   The Swift CPG generation configuration
    * @return
    *   true if the environment has all required tools with compatible versions, false otherwise
    */
  private def isValidEnvironment(config: Config): Boolean = {
    if (config.buildLogPath.isEmpty && !Paths.get(config.inputPath, "Package.swift").toFile.canRead) {
      logger.warn("Package.swift not found or can not be read. This does not look like a valid SwiftPM project.")
      return false
    }

    val commands = if (config.buildLogPath.isDefined) {
      // we do not need 'swift' on the system if the commands are taken from a build log file
      Seq(SwiftCompilerVersionCommand)
    } else {
      SwiftVersionCommands
    }
    val hasSwift = commands.forall { command =>
      findInStdOut(command, _.contains("Swift version ")) match {
        case Some(outLine) =>
          val swiftVersion = outLine.substring(outLine.indexOf("Swift version ") + 14, outLine.indexOf(" ("))
          val isCompatible = Try(VersionHelper.compare(swiftVersion, MinimumSwiftVersion)).toOption.getOrElse(-1) >= 0
          if (!isCompatible) {
            logger.warn(s"Found Swift version '$swiftVersion' but '$MinimumSwiftVersion' is required!")
          }
          isCompatible
        case None =>
          logger.warn("Unable to determine a Swift version on this system!")
          false
      }
    }
    val hasSwiftDemangle = findInStdOut(swiftDemangleVersionCommand()).isDefined
    if (!hasSwiftDemangle) {
      logger.warn("Unable to find swift-demangle on this system!")
    }
    hasSwift && hasSwiftDemangle
  }

  /** Checks whether the given path points to a readable directory.
    *
    * This safely wraps the filesystem checks in `scala.util.Try` and returns `false` if an exception occurs (e.g.,
    * permission errors, broken symlinks).
    *
    * @param path
    *   The filesystem path to check.
    * @return
    *   `true` if `path` exists, is a directory, and is readable; `false` otherwise.
    */
  private def folderExistsAndReadable(path: Path): Boolean = {
    Try(Files.isDirectory(path) && Files.isReadable(path)).getOrElse(false)
  }

  /** Attempts to build a SwiftTypesProvider by running Swift build commands on the project.
    *
    * This method performs the following steps:
    *   - Cleans the Swift project using 'swift package clean'
    *   - Builds the project with 'swift build --verbose' to capture compiler output
    *   - Passes the captured output to the type provider builder
    *
    * @param config
    *   The Swift CPG generation configuration
    * @return
    *   Some(SwiftTypesProvider) if build was successful, None otherwise
    */
  private def build(config: Config): Option[SwiftTypesProvider] = {
    logger.info("Building Swift type map from SwiftPM project configuration")

    val projectPath = Paths.get(config.inputPath)
    logger.info("Cleaning the project first ...")

    if (folderExistsAndReadable(projectPath.resolve(".build/"))) {
      io.shiftleft.semanticcpg.utils.ExternalCommand
        .run(SwiftCleanCommand, workingDir = Some(projectPath))
        .logIfFailed()
    }

    logger.info("Building the project ...")
    io.shiftleft.semanticcpg.utils.ExternalCommand
      .run(SwiftBuildCommand, mergeStdErrInStdOut = true, workingDir = Some(projectPath))
      .logIfFailed()
      .successOption
      .map(outLines => build(config, outLines))
  }

  /** Determines whether a Swift compiler argument should be filtered out from processing.
    *
    * @param arg
    *   The compiler argument to check
    * @return
    *   true if the argument should be filtered out, false otherwise
    */
  private def argShouldBeFiltered(arg: String): Boolean = {
    SwiftCompilerIgnoredArgs.contains(arg) || arg.startsWith("-emit")
  }

  /** Parses and filters Swift compiler arguments, removing ignored arguments and their associated values.
    *
    * This method processes a list of Swift compiler arguments, removing those that should be ignored based on the
    * SwiftCompilerIgnoredArgs list. For arguments that have associated values, it also removes those values from the
    * resulting list.
    *
    * @param args
    *   The list of Swift compiler arguments to process
    * @return
    *   A filtered list of Swift compiler arguments
    */
  private def parseSwiftCompilerArgs(args: List[String]): List[String] = {
    @scala.annotation.tailrec
    def loop(remaining: List[String], result: List[String]): List[String] = {
      remaining match {
        case Nil                                              => result.reverse
        case "--" :: tail                                     => loop(tail, result)
        case "" :: tail                                       => loop(tail, result)
        case "builtin-Swift-Compilation" :: tail              => loop(tail, result)
        case "builtin-SwiftDriver" :: tail                    => loop(tail, result)
        case "builtin-Swift-Compilation-Requirements" :: tail => loop(tail, result)
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

  /** Determines if a command line represents a Swift compiler invocation.
    *
    * This method checks if the given line contains characteristic patterns of a Swift compiler invocation, specifically
    * the swiftc executable with a module name parameter, but excluding executable emit commands.
    *
    * @param line
    *   The command line to check
    * @return
    *   true if the line represents a Swift compiler invocation, false otherwise
    */
  private def isSwiftInvocation(line: String): Boolean = {
    (line.contains("\\swiftc.exe ") || line.contains("/swiftc "))
    && line.contains(" -module-name ")
    && !line.contains(" -emit-executable ")
  }

  /** Splits a command line string into individual arguments, preserving escaped spaces.
    *
    * This method splits a command line using the SwiftCompilerArgSplit regex pattern, which handles backslash-escaped
    * spaces properly, ensuring spaces within arguments don't cause incorrect splitting.
    *
    * @param line
    *   The command line to split
    * @return
    *   A list of individual arguments
    */
  private def argsFromLine(line: String): List[String] = {
    line.replace("\\ -", " -").replace("/ -", " -").replace("\\=", "=").split(SwiftCompilerArgSplit).toList
  }

  /** Lists all readable directories under the given path.
    *
    * @param path
    *   The starting directory path to search from
    * @return
    *   A set of names of the readable directories found under path
    */
  private def listReadableDirectories(path: Path): Set[String] = {
    if (path.toString.isEmpty || !folderExistsAndReadable(path)) {
      return Set.empty
    }
    try {
      val resultSet = mutable.Set.empty[String]
      Files.walkFileTree(
        path,
        java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS),
        // 3 = The directory itself + all directories top-level + all directories under them (as these indicate modules).
        // We could use the Package.swift module description but that is only available in SwiftPM projects, and
        // we want to support as many project types as possible.
        3,
        new SimpleFileVisitor[Path] {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (dir.getFileName.toString.startsWith(".")) {
              return FileVisitResult.SKIP_SUBTREE
            }
            if (Files.isReadable(dir)) {
              dir.nameOption.foreach(resultSet.add)
            }
            FileVisitResult.CONTINUE
          }
        }
      )
      resultSet.toSet
    } catch {
      case e: Exception =>
        logger.warn(s"Error listing module directories under ${path.toFile.toString}: ${e.getMessage}")
        Set.empty
    }
  }

  /** Builds a SwiftTypesProvider by parsing and processing compiler output.
    *
    * This method processes the output from Swift compiler invocations to extract type information:
    *   - It identifies source modules from the project directory structure
    *   - It filters compiler output for relevant Swift compiler invocations
    *   - It parses these invocations and adds AST dump options for type extraction
    *   - It filters to only include invocations for modules found in the source directory
    *
    * @param config
    *   The CPG generation configuration
    * @param compilerOutput
    *   The output lines from a Swift build process or build log
    * @return
    *   A SwiftTypesProvider initialized with parsed compiler invocations
    */
  def build(config: Config, compilerOutput: Seq[String]): SwiftTypesProvider = {
    config.buildLogPath.foreach(path =>
      logger.info(s"Building Swift type map from compiler log at ${path.toFile.toString}")
    )

    val sourceModules    = listReadableDirectories(Paths.get(config.inputPath))
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

  private val mangledNameCache = java.util.concurrent.ConcurrentHashMap[String, Option[String]]()

  /** Demangles a Swift symbol name to a human-readable form.
    *
    * This method invokes the swift-demangle tool to convert mangled Swift symbol names into their original,
    * human-readable form. It strips certain prefixes and replaces specific patterns before demangling.
    *
    * @param mangledName
    *   The mangled Swift symbol name to demangle
    * @return
    *   Some(demangled) if successful, None otherwise
    */
  private def demangle(mangledName: String): Option[String] = {
    if (mangledName.isEmpty) return None

    // special handling for objc symbols
    if (mangledName.contains("@objc")) {
      // `c:@(C)M@` is used as module prefix for objc symbols
      // `@objc(cs)` indicates that a type follows that is an objc class
      // `(im)` indicates that an instance method follows
      return Some(mangledName.stripPrefix("c:@M@").stripPrefix("c:@CM@").replace("@objc(cs)", ".").replace("(im)", "."))
    }

    val strippedMangledName = mangledName.stripPrefix("$").replace("s:e:s:", "s:").replace(":", "")
    findInStdOut(swiftDemangleCommand :+ strippedMangledName).map(_.trim)
  }

  /** Removes Swift modifiers from a fullName.
    *
    * This method removes any Swift declaration modifiers (like public, private, static) from the given fullName,
    * cleaning it up for display and analysis.
    *
    * @param fullName
    *   The fullName potentially containing Swift modifiers
    * @return
    *   The cleaned fullName with modifiers removed
    */
  private def removeModifier(fullName: String): String = {
    SwiftDeclModifier.foldLeft(fullName) { (cur, repl) =>
      if (cur.startsWith(s"$repl ") || cur.contains(s" $repl ")) {
        cur.replace(s" $repl ", " ").stripPrefix(s"$repl ")
      } else cur
    }
  }

  /** Calculates a demangled type fullName from a mangled Swift type name.
    *
    * This method demangles the Swift type name, removes modifiers, strips generics, and removes spaces to create a
    * clean, fully qualified type name. Results are cached.
    *
    * @param mangledName
    *   The mangled Swift type name
    * @return
    *   Some(demangled type fullName) if successful, None otherwise
    */
  private def calculateTypeFullname(mangledName: String): Option[String] = {
    calculateDeclFullname(mangledName).map(_.stripSuffix(".Type"))
  }

  /** This regex is designed to clean up Swift demangled type names by extracting meaningful parts and removing internal
    * representation details. It has three capture groups:
    *
    *   1. `([^(]+)` - Captures everything before the first opening parenthesis, Example:
    *      `SwiftHelloWorldLib.HelloWorld.`
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

  /** Regex to match demangled initializer-like signatures that contain a `->` return type followed by an `(in ...)`
    * clause and a member after a dot.
    *
    * Example match (abstracted): `... ) -> Module.Type(in _ID...).memberRest`
    *
    * Capture groups:
    *   1. `(.+)` — the module/type name before the `\(in` clause (e.g. `Module.Type`)
    *   1. `(.+)` — the member or remainder after the final dot (e.g. `memberRest`)
    *
    * Structure breakdown:
    *   - `.+\)` : any characters ending with a closing parenthesis (end of parameter list)
    *   - `\s->\s` : literal arrow with surrounding spaces
    *   - `(.+)\(in.+\)` : capture the module/type name that precedes the `(in ...)` clause
    *   - `\.(.+)` : a dot followed by the captured member/signature tail
    */
  private val InitNameRegex: Regex = """.+\)\s->\s(.+)\(in.+\)\.(.+)""".r

  /** Basically the same as with MemberNameRegex. But here for extension function fullNames.
    *
    * E.g., `(extension in FooExt):Foo.bar() -> Swift.String` becomes `FooExt.Foo.bar() -> Swift.String`.
    */
  private val ExtensionNameRegex: Regex = """^\(extension\sin\s([^)]+)\):(.*)""".r

  private val ExtensionInSignatureRegex: Regex = """\(extension in ([^)]+)\):""".r

  /** Calculates a demangled declaration fullName from a mangled Swift declaration name.
    *
    * This method demangles the Swift declaration name, applies special transformations for member names and extensions,
    * and cleans up the result by removing modifiers and spaces. Results are cached.
    *
    * @param mangledName
    *   The mangled Swift declaration name
    * @return
    *   Some(demangled declaration fullName) if successful, None otherwise
    */
  private def calculateDeclFullname(mangledName: String): Option[String] = {
    mangledNameCache.computeIfAbsent(
      mangledName,
      _ =>
        demangle(mangledName)
          .map {
            case ExtensionNameRegex(name, rest) =>
              removeModifier(s"$name<extension>.${rest.stripPrefix(s"$name.")}")
            case MemberNameRegex(parent, name, rest) =>
              AstCreatorHelper.stripGenerics(removeModifier(s"$parent$name$rest"))
            case InitNameRegex(name, rest) =>
              AstCreatorHelper.stripGenerics(removeModifier(s"$name.$rest"))
            case other =>
              AstCreatorHelper.stripGenerics(removeModifier(other))
          }
          .map { fullName =>
            val withExtensionsFixed = replaceExtensionInSignature(fullName)
            withExtensionsFixed.replace(" ", "")
          }
    )
  }

  /** Rewrites `(extension in X):` fragments that can appear inside demangled Swift signatures. Swift demangling may
    * embed extension context qualifiers like: `(...(extension in FooExt):Foo.bar...)` This method normalizes those
    * occurrences by replacing: `(extension in FooExt):Foo.` with `FooExt.Foo.`
    *
    * It repeatedly applies the rewrite until no such fragment remains, ensuring nested or multiple occurrences are
    * handled.
    *
    * @param fullName
    *   The demangled fullName that may contain extension qualifiers.
    * @return
    *   The normalized fullName with extension qualifiers rewritten.
    */
  @scala.annotation.tailrec
  private[utils] final def replaceExtensionInSignature(fullName: String): String = {
    ExtensionInSignatureRegex.findFirstMatchIn(fullName) match {
      case Some(m) =>
        val head     = m.before
        val name     = m.group(1)
        val tail     = m.after.toString
        val rest     = tail.stripPrefix(s"$name.")
        val replaced = s"$head$name.$rest"
        replaceExtensionInSignature(replaced)
      case None =>
        fullName
    }
  }

  /** Resolves a TypeInfo object to a ResolvedTypeInfo by demangling type and declaration fullNames.
    *
    * @param typeInfo
    *   The TypeInfo object to resolve
    * @return
    *   A ResolvedTypeInfo object containing demangled fullNames
    */
  private def resolve(typeInfo: TypeInfo): ResolvedTypeInfo = {
    val demangledTypeFullname = typeInfo.typeFullname.flatMap(calculateTypeFullname)
    val demangledDeclFullname = typeInfo.declFullname.flatMap(calculateDeclFullname)
    val demangledInherits     = typeInfo.inherits.flatMap(calculateTypeFullname)
    ResolvedTypeInfo(demangledTypeFullname, demangledDeclFullname, demangledInherits, typeInfo.nodeKind)
  }

  /** Parses Swift compiler JSON output and adds type information to the provided type mapping.
    *
    * This method reads type information from the Swift compiler's JSON output, resolves the mangled names to their
    * demangled forms, and populates the type mapping.
    *
    * @param jsonString
    *   The JSON string from the Swift compiler
    * @param result
    *   The type mapping to update with extracted information
    */
  def mappingFromJson(jsonString: String, result: MutableSwiftTypeMapping): Unit = {
    Using.resource(new StringReader(jsonString)) { reader =>
      GsonTypeInfoReader.collectTypeInfo(reader).foreach { typeInfo =>
        val range    = typeInfo.range
        val filename = typeInfo.filename
        result.compute(
          filename,
          {
            case (_, null) =>
              logger.debug(s"Generating type map for: $filename")
              val rangeMapping = new MutableSwiftFileLocalTypeMapping()
              rangeMapping.put(range, new mutable.HashSet[ResolvedTypeInfo]().addOne(resolve(typeInfo)))
              rangeMapping
            case (_, rangeMapping) =>
              rangeMapping.compute(
                range,
                {
                  case (_, null) => new mutable.HashSet[ResolvedTypeInfo]().addOne(resolve(typeInfo))
                  case (_, set)  => set.addOne(resolve(typeInfo))
                }
              )
              rangeMapping
          }
        )
      }
    }
  }

  /** Retrieves Swift type mappings by executing Swift compiler commands and processing output.
    *
    * This method executes all parsed Swift compiler invocations in parallel, processes the JSON output to extract type
    * information, and builds a comprehensive type mapping. Progress and errors are logged appropriately.
    *
    * @return
    *   A Map containing filename-to-type mappings for Swift source files
    */
  def retrieveMappings(): SwiftTypeMapping = {
    val mapping = new MutableSwiftTypeMapping
    // We want to use the same pool for parallel Swift compiler invocations and their type mapping work
    val pool = java.util.concurrent.Executors.newCachedThreadPool()
    try {
      val parInvocations = parsedSwiftInvocations.par
      val ec             = ExecutionContext.fromExecutorService(pool)
      parInvocations.tasksupport = new ExecutionContextTaskSupport(ec)
      parInvocations.foreach { invocationCommand =>
        Using.Manager { use =>
          val reader = use(new InputStreamReader(inputStreamFromCommand(invocationCommand, config.inputPath)))
          val stdOut = use(new BufferedReader(reader))
          ParallelLineProcessor.processLinesParallel(stdOut, pool, _.startsWith("{"))(jsonString =>
            mappingFromJson(jsonString, mapping)
          )
        } match {
          case Failure(exception) =>
            // Using.Manager swallows exceptions otherwise
            logger.warn("Error during Swift type map creation", exception)
          case _ => // this is fine
        }
      }
      logger.info(s"Got ${mapping.size} type map entries.")
      mapping.asScala.toMap.map { case (filename, mapping) =>
        filename -> mapping.asScala.toMap.map { case (range, set) =>
          range -> set.toSet
        }
      }
    } finally {
      pool.shutdown()
    }
  }

}
