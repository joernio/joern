/** originally from amm/compiler/src/main/scala-3/ammonite/compiler/Compiler.scala  */
package ammonite.foo

import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.io.{ByteArrayInputStream, OutputStream}
import dotty.tools.dotc
import dotc.{CompilationUnit, Run, ScalacCommand, Compiler as DottyCompiler}
import dotc.ast.{tpd, untpd}
import dotc.ast.Positioned
import dotc.classpath
import dotc.config.{CompilerCommand, JavaPlatform}
import dotc.core.Contexts.*
import dotc.core.{Flags, MacroClassLoader, Mode}
import dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotc.core.Phases.{Phase, unfusedPhases}
import dotc.core.Symbols.{Symbol, defn}
import dotc.fromtasty.TastyFileUtil
import dotc.interactive.Completion
import dotc.report
import dotc.reporting
import dotc.semanticdb
import dotc.transform.{PostTyper, Staging}
import dotc.util.{Property, SourceFile, SourcePosition}
import dotc.util.Spans.Span
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation, File, Path, PlainFile, VirtualDirectory, VirtualFile}
import dotty.tools.repl.CollectTopLevelImports

class Compiler(
  val dynamicClassPath: AbstractFile,
  val initialClassPath: Seq[URL],
  val classPath: Seq[URL],
  val whiteList: Set[Seq[String]],
  dependencyCompleteOpt: => Option[String => (Int, Seq[String])] = None,
  val contextInit: FreshContext => Unit = _ => (),
  val settings: Seq[String] = Nil,
  val reporter: Option[Compiler.Message => Unit] = None) { self =>
  import Compiler.{enumerateVdFiles, files}
  import Util.newLine

  private val outputDir = new VirtualDirectory("(memory)")

  private def initCtx: Context =
    val base: ContextBase =
      new ContextBase :
        override protected def newPlatform(using Context) =
          new JavaPlatform :
            private var classPath0: ClassPath = null

            override def classPath(using Context) =
              if (classPath0 == null) {
//                println(s"XXX1 classpath0.contains(versionsort)=${self.classPath.find(_.getPath.contains("versionsort")).isDefined}")
                classPath0 = classpath.AggregateClassPath(Seq(
                  asDottyClassPath(initialClassPath, whiteListed = true),
                  asDottyClassPath(self.classPath),
                  classpath.ClassPathFactory.newClassPath(dynamicClassPath)
                ))
              }
              classPath0
    base.initialCtx

  private def sourcesRequired = false

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/dotc/Driver.scala/#L67-L81
  private def setup(args: Array[String], rootCtx: Context): (List[String], Context) =
    given ictx: FreshContext = rootCtx.fresh

    val summary = ScalacCommand.distill(args, ictx.settings)(ictx.settingsState)(using ictx)
    ictx.setSettings(summary.sstate)
    Positioned.init

    if !ictx.settings.YdropComments.value then
      ictx.setProperty(ContextDoc, new ContextDocstrings)
    val fileNamesOpt = ScalacCommand.checkUsage(
      summary,
      sourcesRequired
    )(using ictx.settings)(using ictx.settingsState)
    val fileNames = fileNamesOpt.getOrElse {
      throw new Exception("Error initializing compiler")
    }
    contextInit(ictx)
    (fileNames, ictx)

  private def asDottyClassPath(
                                cp: Seq[URL],
                                whiteListed: Boolean = false
                              )(using Context): ClassPath =
    val (dirs, jars) = cp.partition { url =>
      url.getProtocol == "file" && Files.isDirectory(Paths.get(url.toURI))
    }

    val dirsCp = dirs.map(u => classpath.ClassPathFactory.newClassPath(AbstractFile.getURL(u)))
    val jarsCp = jars
      .filter(Classpath.canBeOpenedAsJar)
      .map(u => classpath.ZipAndJarClassPathFactory.create(AbstractFile.getURL(u)))

    if (whiteListed) new dotty.ammonite.foo.WhiteListClasspath(dirsCp ++ jarsCp, whiteList)
    else classpath.AggregateClassPath(dirsCp ++ jarsCp)

  /** Create a fresh and initialized context with IDE mode enabled */
  lazy val initialCtx =
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.YcookComments, true)
    rootCtx.setSetting(rootCtx.settings.outputDir, outputDir)

    val (_, ictx) = setup(settings.toArray, rootCtx)
    ictx.base.initialize()(using ictx)
    ictx

  private var userCodeNestingLevel = -1

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ReplCompiler.scala/#L34-L39
  val compiler = new DottyCompiler {
    override protected def frontendPhases: List[List[Phase]] =
      CompilerHelper.frontEndPhases ++
        List(
          List(new semanticdb.ExtractSemanticDB),
          List(new AmmonitePhase(userCodeNestingLevel, userCodeNestingLevel == 2)),
          List(new PostTyper)
        )
  }

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/Rendering.scala/#L97-L103

  /** Formats errors using the `messageRenderer` */
  private def formatError(dia: reporting.Diagnostic)(implicit ctx: Context): reporting.Diagnostic =
    new reporting.Diagnostic(
      CompilerHelper.messageAndPos(Compiler.messageRenderer, dia),
      dia.pos,
      dia.level
    )

  def compile(src: Array[Byte],
              printer: Printer,
              importsLen: Int,
              userCodeNestingLevel: Int,
              fileName: String): Option[Compiler.Output] = {
    // println(s"Compiling\n${new String(src, StandardCharsets.UTF_8)}\n")

    self.userCodeNestingLevel = userCodeNestingLevel

    val reporter0 = reporter match {
      case None =>
        Compiler.newStoreReporter()
      case Some(rep) =>
        val simpleReporter = new dotc.interfaces.SimpleReporter {
          def report(diag: dotc.interfaces.Diagnostic) = {
            val severity = diag.level match {
              case dotc.interfaces.Diagnostic.ERROR => "ERROR"
              case dotc.interfaces.Diagnostic.WARNING => "WARNING"
              case dotc.interfaces.Diagnostic.INFO => "INFO"
              case _ => "INFO" // should not happen
            }
            val pos = Some(diag.position).filter(_.isPresent).map(_.get)
            val start = pos.fold(0)(_.start)
            val end = pos.fold(new String(src, "UTF-8").length)(_.end)
            val msg = Compiler.Message(severity, start, end, diag.message)
            rep(msg)
          }
        }
        reporting.Reporter.fromSimpleReporter(simpleReporter)
    }
    val run = new Run(compiler, initialCtx.fresh.setReporter(reporter0))

    val semanticDbEnabled = run.runContext.settings.Xsemanticdb.value(using run.runContext)
    val sourceFile =
      if (semanticDbEnabled) {
        // semanticdb needs the sources to be written on disk, so we assume they're there already
        val root = run.runContext.settings.sourceroot.value(using run.runContext)
        SourceFile(AbstractFile.getFile(Paths.get(root).resolve(fileName)), "UTF-8")
      } else {
        val vf = new VirtualFile(fileName.split("/", -1).last, fileName)
        val out = vf.output
        out.write(src)
        out.close()
        new SourceFile(vf, new String(src, "UTF-8").toCharArray)
      }

    implicit val ctx: Context = run.runContext.withSource(sourceFile)
    val unit = new CompilationUnit(ctx.source) {
      // as done in
      // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
      //   compiler/src/dotty/tools/repl/ReplCompillationUnit.scala/#L8
      override def isSuspendable: Boolean = false
    }
    ctx.run.compileUnits(unit :: Nil)

    val result =
      if (ctx.reporter.hasErrors) Left(reporter.fold(ctx.reporter.removeBufferedMessages)(_ => Nil))
      else Right(unit)

    result match {
      case Left(errors) =>
        val scalaPosToScPos = PositionOffsetConversion.scalaPosToScPos(
          new String(src).drop(importsLen),
          0,
          0,
          new String(src),
          importsLen
        )
        val scFile = new SourceFile(sourceFile.file, sourceFile.content().drop(importsLen))

        def scalaOffsetToScOffset(scalaOffset: Int): Option[Int] =
          scalaPosToScPos(sourceFile.offsetToLine(scalaOffset), sourceFile.column(scalaOffset)).map {
            case (scLine, scCol) => scFile.lineToOffset(scLine) + scCol
          }

        def scalaSpanToScSpan(scalaSpan: Span): Option[Span] =
          for {
            scStart <- scalaOffsetToScOffset(scalaSpan.start)
            scEnd <- scalaOffsetToScOffset(scalaSpan.end)
            scPoint <- scalaOffsetToScOffset(scalaSpan.point)
          } yield Span(scStart, scEnd, scPoint)

        def scalaSourcePosToScSourcePos(sourcePos: SourcePosition): Option[SourcePosition] =
          if (sourcePos.source == sourceFile)
            scalaSpanToScSpan(sourcePos.span).map { scSpan =>
              SourcePosition(scFile, scSpan, sourcePos.outer)
            }
          else
            None

        def scalaDiagnosticToScDiagnostic(diag: reporting.Diagnostic): Option[reporting.Diagnostic] =
          scalaSourcePosToScSourcePos(diag.pos).map { scPos =>
            new reporting.Diagnostic(diag.msg, scPos, diag.level)
          }

        errors
          .map(d => scalaDiagnosticToScDiagnostic(d).getOrElse(d))
          .map(formatError)
          .map(_.msg.toString)
          .foreach(printer.error)
        None
      case Right(unit) =>
        val newImports = unfusedPhases.collectFirst {
          case p: AmmonitePhase => p.importData
        }.getOrElse(Seq.empty[ImportData])
        val usedEarlierDefinitions = unfusedPhases.collectFirst {
          case p: AmmonitePhase => p.usedEarlierDefinitions
        }.getOrElse(Seq.empty[String])
        val fileCount = enumerateVdFiles(outputDir).length
        val classes = files(outputDir).toArray
        Compiler.addToClasspath(classes, dynamicClassPath)
        outputDir.clear()
        val lineShift = PositionOffsetConversion.offsetToPos(new String(src)).apply(importsLen).line
        val mappings = Map(sourceFile.file.name -> (sourceFile.file.name, -lineShift))
        val postProcessedClasses = classes.toVector.map {
          case (path, byteCode) if path.endsWith(".class") =>
            val updatedByteCodeOpt = AsmPositionUpdater.postProcess(
              mappings,
              new ByteArrayInputStream(byteCode)
            )
            (path, updatedByteCodeOpt.getOrElse(byteCode))
          case other =>
            other
        }
        Some(Compiler.Output(
          postProcessedClasses,
          Imports(newImports),
          Some(usedEarlierDefinitions)
        ))
    }
  }

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ReplCompiler.scala/#L224-L286
  def tryTypeCheck(src: Array[Byte], fileName: String): (Tree, Context) =
    val sourceFile = SourceFile.virtual(fileName, new String(src, StandardCharsets.UTF_8))

    val reporter0 = Compiler.newStoreReporter()
    val run = new Run(
      compiler,
      initialCtx.fresh
        .addMode(Mode.ReadPositions | Mode.Interactive)
        .setReporter(reporter0)
        .setSetting(initialCtx.settings.YstopAfter, List("typer"))
    )
    implicit val ctx: Context = run.runContext.withSource(sourceFile)

    val unit =
      new CompilationUnit(ctx.source) :
        override def isSuspendable: Boolean = false
    ctx
      .run
      .compileUnits(unit :: Nil, ctx)

    (unit.tpdTree, ctx)

  def complete(
                offset: Int,
                previousImports: String,
                snippet: String
              ): (Int, Seq[String], Seq[String]) = {

    val prefix = previousImports + newLine +
      "object AutocompleteWrapper{ val expr: _root_.scala.Unit = {" + newLine
    val suffix = newLine + "()}}"
    val allCode = prefix + snippet + suffix
    val index = offset + prefix.length


    // Originally based on
    // https://github.com/lampepfl/dotty/blob/3.0.0-M1/
    //   compiler/src/dotty/tools/repl/ReplDriver.scala/#L179-L191

    val (tree, ctx0) = tryTypeCheck(allCode.getBytes("UTF-8"), "<completions>")
    val ctx = ctx0.fresh
    val file = SourceFile.virtual("<completions>", allCode, maybeIncomplete = true)
    val unit = CompilationUnit(file)(using ctx)
    unit.tpdTree = {
      given Context = ctx
      import tpd._
      tree match {
        case PackageDef(_, p) =>
          p.collectFirst {
            case TypeDef(_, tmpl: Template) =>
              tmpl.body
                .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd }
                .getOrElse(???)
          }.getOrElse(???)
        case _ => ???
      }
    }
    val ctx1 = ctx.fresh.setCompilationUnit(unit)
    val srcPos = SourcePosition(file, Span(index))
    val (start, completions) = dotty.ammonite.foo.AmmCompletion.completions(
      srcPos,
      dependencyCompleteOpt = dependencyCompleteOpt,
      enableDeep = false
    )(using ctx1)

    val blacklistedPackages = Set("shaded")

    def deepCompletion(name: String): List[String] = {
      given Context = ctx1

      def rec(t: Symbol): Seq[Symbol] = {
        if (blacklistedPackages(t.name.toString))
          Nil
        else {
          val children =
            if (t.is(Flags.Package) || t.is(Flags.PackageVal) || t.is(Flags.PackageClass))
              t.denot.info.allMembers.map(_.symbol).filter(_ != t).flatMap(rec)
            else Nil

          t +: children.toSeq
        }
      }

      for {
        member <- defn.RootClass.denot.info.allMembers.map(_.symbol).toList
        sym <- rec(member)
        // Scala 2 comment: sketchy name munging because I don't know how to do this properly
        // Note lack of back-quoting support.
        strippedName = sym.name.toString.stripPrefix("package$").stripSuffix("$")
        if strippedName.startsWith(name)
        (pref, _) = sym.fullName.toString.splitAt(sym.fullName.toString.lastIndexOf('.') + 1)
        out = pref + strippedName
        if out != ""
      } yield out
    }

    def blacklisted(s: Symbol) = {
      given Context = ctx1

      val blacklist = Set(
        "scala.Predef.any2stringadd.+",
        "scala.Any.##",
        "java.lang.Object.##",
        "scala.<byname>",
        "scala.<empty>",
        "scala.<repeated>",
        "scala.<repeated...>",
        "scala.Predef.StringFormat.formatted",
        "scala.Predef.Ensuring.ensuring",
        "scala.Predef.ArrowAssoc.->",
        "scala.Predef.ArrowAssoc.→",
        "java.lang.Object.synchronized",
        "java.lang.Object.ne",
        "java.lang.Object.eq",
        "java.lang.Object.wait",
        "java.lang.Object.notifyAll",
        "java.lang.Object.notify",
        "java.lang.Object.clone",
        "java.lang.Object.finalize"
      )

      blacklist(s.showFullName) ||
        s.isOneOf(Flags.GivenOrImplicit) ||
        // Cache objects, which you should probably never need to
        // access directly, and apart from that have annoyingly long names
        "cache[a-f0-9]{32}".r.findPrefixMatchOf(s.name.decode.toString).isDefined ||
        // s.isDeprecated ||
        s.name.decode.toString == "<init>" ||
        s.name.decode.toString.contains('$')
    }

    val filteredCompletions = completions.filter { c =>
      c.symbols.isEmpty || c.symbols.exists(!blacklisted(_))
    }
    val signatures = {
      given Context = ctx1

      for {
        c <- filteredCompletions
        s <- c.symbols
        isMethod = s.denot.is(Flags.Method)
        if isMethod
      } yield s"def ${s.name}${s.denot.info.widenTermRefExpr.show}"
    }
    (start - prefix.length, filteredCompletions.map(_.label.replace(".package$.", ".")), signatures)
  }
}

object Compiler {

  /** Create empty outer store reporter */
  def newStoreReporter(): reporting.StoreReporter =
    new reporting.StoreReporter(null)
      with reporting.UniqueMessagePositions with reporting.HideNonSensicalMessages

  private def enumerateVdFiles(d: VirtualDirectory): Iterator[AbstractFile] =
    val (subs, files) = d.iterator.partition(_.isDirectory)
    files ++ subs.map(_.asInstanceOf[VirtualDirectory]).flatMap(enumerateVdFiles)

  private def files(d: VirtualDirectory): Iterator[(String, Array[Byte])] =
    for (x <- enumerateVdFiles(d) if x.name.endsWith(".class") || x.name.endsWith(".tasty")) yield {
      val segments = x.path.split("/").toList.tail
      (x.path.stripPrefix("(memory)/"), x.toByteArray)
    }

  private def writeDeep(d: AbstractFile, path: List[String]): OutputStream = path match {
    case head :: Nil => d.fileNamed(path.head).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head), //.asInstanceOf[VirtualDirectory],
        rest
      )
    // We should never write to an empty path, and one of the above cases
    // should catch this and return before getting here
    case Nil => ???
  }

  def addToClasspath(classFiles: Iterable[(String, Array[Byte])],
                     dynamicClasspath: AbstractFile): Unit = {
    for ((name, bytes) <- classFiles) {
//      println(s"XX1 adding to classpath: $name") // XX1 adding to classpath: ammonite/$sess/cmd1.class ...
      val output = writeDeep(dynamicClasspath, name.split('/').toList)
      output.write(bytes)
      output.close()
    }

  }

  private val messageRenderer =
    new reporting.MessageRendering {}

  /** originally from amm/compiler/interface/src/main/scala/ammonite/compiler/iface/Compiler.scala */
  case class Output(classFiles: Vector[(String, Array[Byte])],
                    imports: Imports,
                    usedEarlierDefinitions: Option[Seq[String]])

  /** originally from amm/compiler/interface/src/main/scala/ammonite/compiler/iface/CompilerBuilder.scala */
  case class Message(severity: String,
                     start: Int,
                     end: Int,
                     message: String)

}
