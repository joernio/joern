package io.joern.ghidra2cpg

import ghidra.GhidraJarApplicationLayout
import ghidra.app.decompiler.{DecompInterface, DecompileOptions}
import ghidra.app.plugin.core.analysis.AutoAnalysisManager
import ghidra.app.util.importer.{AutoImporter, MessageLog}
import ghidra.framework.model.{Project, ProjectLocator}
import ghidra.framework.project.{DefaultProject, DefaultProjectManager}
import ghidra.framework.protocol.ghidra.{GhidraURLConnection, Handler}
import ghidra.framework.{Application, HeadlessGhidraApplicationConfiguration}
import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.model.listing.Program
import ghidra.program.util.GhidraProgramUtilities
import ghidra.util.exception.InvalidInputException
import ghidra.util.task.TaskMonitor
import io.joern.ghidra2cpg.passes._
import io.joern.ghidra2cpg.processors._
import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.x2cpg.X2Cpg
import org.apache.commons.io.FileUtils
import utilities.util.FileUtilities

import java.io.File
import java.nio.file.Files
import scala.collection.mutable
import scala.jdk.CollectionConverters._
object Types {

  // Types will be added to the CPG as soon as everything
  // else is done
  val types: mutable.SortedSet[String] = scala.collection.mutable.SortedSet[String]()
  def registerType(typeName: String): String = {
    types += typeName
    typeName
  }
}
class Ghidra2Cpg(
    inputFile: File,
    outputFile: Option[String]
) {

  val tempWorkingDir: File = Files.createTempDirectory("ghidra2cpg_tmp").toFile
  // tempWorkingDir.deleteOnExit() is not reliable,
  // adding a shutdown hook seems to work https://stackoverflow.com/posts/35212952/revisions
  Runtime.getRuntime.addShutdownHook(new Thread(() => FileUtils.deleteQuietly(tempWorkingDir)))

  def createCpg(): Unit = {
    // We need this for the URL handler
    Handler.registerHandler()

    var projectManager: Option[HeadlessGhidraProjectManager] =
      None: Option[HeadlessGhidraProjectManager]

    var project: Option[Project] = None
    // Initialize application (if necessary and only once)
    if (!Application.isInitialized) {
      val configuration = new HeadlessGhidraApplicationConfiguration
      configuration.setInitializeLogging(false)
      Application.initializeApplication(new GhidraJarApplicationLayout, configuration)
    }

    if (!inputFile.isDirectory && !inputFile.isFile)
      throw new InvalidInputException(
        s"$inputFile is not a valid directory or file."
      )

    val locator = new ProjectLocator(tempWorkingDir.getAbsolutePath, CommandLineConfig.projectName)
    var program: Program = null
    try {
      projectManager = Some(new HeadlessGhidraProjectManager)
      project = Some(projectManager.get.createProject(locator, null, false))
      program = AutoImporter.importByUsingBestGuess(
        inputFile,
        null,
        this,
        new MessageLog,
        TaskMonitor.DUMMY
      )

      analyzeProgram(inputFile.getAbsolutePath, program)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    } finally {
      if (program != null) {
        AutoAnalysisManager.getAnalysisManager(program).dispose()
        program.release(this)
        program = null
      }
      project.get.close()
      // Used to have this in a config but we delete the directory anyway
      // if (!config.runScriptsNoImport && config.deleteProject)
      FileUtilities.deleteDir(locator.getProjectDir)
      locator.getMarkerFile.delete

    }
  }
  private def analyzeProgram(fileAbsolutePath: String, program: Program): Unit = {
    val autoAnalysisManager: AutoAnalysisManager = AutoAnalysisManager.getAnalysisManager(program)
    val transactionId: Int = program.startTransaction("Analysis")
    try {
      autoAnalysisManager.initializeOptions()
      autoAnalysisManager.reAnalyzeAll(null)
      autoAnalysisManager.startAnalysis(TaskMonitor.DUMMY)
      GhidraProgramUtilities.setAnalyzedFlag(program, true)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    } finally {
      program.endTransaction(transactionId, true)
    }
    try {
      handleProgram(program, fileAbsolutePath)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    }
  }

  def handleProgram(currentProgram: Program, fileAbsolutePath: String): Unit = {

    val flatProgramAPI: FlatProgramAPI = new FlatProgramAPI(currentProgram)
    val decompilerInterface = new DecompInterface()
    decompilerInterface.toggleCCode(false)
    decompilerInterface.toggleSyntaxTree(false)
    decompilerInterface.toggleJumpLoads(false)
    decompilerInterface.toggleParamMeasures(true)
    decompilerInterface.setSimplificationStyle("decompile")

    val opts = new DecompileOptions()

    opts.grabFromProgram(currentProgram)
    decompilerInterface.setOptions(opts)

    println(s"""[ + ] Starting CPG generation""")
    if (!decompilerInterface.openProgram(currentProgram)) {
      println("Decompiler error: %s\n", decompilerInterface.getLastMessage)
    }
    // Functions
    val listing = currentProgram.getListing
    val functionIterator = listing.getFunctions(true)
    val functions = functionIterator.iterator.asScala.toList

    // We touch every function twice, regular ASM and PCode
    // Also we have + 2 for MetaDataPass and Namespacepass
    val numOfKeypools = functions.size * 3 + 2
    val keyPoolIterator = KeyPoolCreator.obtain(numOfKeypools).iterator

    // Actual CPG construction
    val cpg = X2Cpg.newEmptyCpg(outputFile)

    new MetaDataPass(fileAbsolutePath, cpg, keyPoolIterator.next()).createAndApply()
    new NamespacePass(cpg, fileAbsolutePath, keyPoolIterator.next()).createAndApply()

    currentProgram.getLanguage.getLanguageDescription.getProcessor.toString match {
      case "MIPS" =>
        functions.foreach { function =>
          new MipsFunctionPass(
            new MipsProcessor,
            currentProgram,
            fileAbsolutePath,
            functions,
            function,
            cpg,
            keyPoolIterator.next(),
            decompilerInterface
          ).createAndApply()
          new LoHiPass(cpg)
        }
      case "AARCH64" =>
        functions.foreach { function =>
          new ArmFunctionPass(
            new ArmProcessor,
            currentProgram,
            fileAbsolutePath,
            functions,
            function,
            cpg,
            keyPoolIterator.next(),
            decompilerInterface
          ).createAndApply()
        }
      case _ =>
        functions.foreach { function =>
          new X86FunctionPass(
            new X86Processor,
            currentProgram,
            fileAbsolutePath,
            functions,
            function,
            cpg,
            keyPoolIterator.next(),
            decompilerInterface
          ).createAndApply()
        }
    }

    new TypesPass(cpg).createAndApply()
    new JumpPass(cpg, keyPoolIterator.next()).createAndApply()
    new LiteralPass(cpg, currentProgram, flatProgramAPI, keyPoolIterator.next()).createAndApply()
    cpg.close()
  }

  private class HeadlessProjectConnection(
      projectManager: HeadlessGhidraProjectManager,
      connection: GhidraURLConnection
  ) extends DefaultProject(projectManager, connection) {}

  private class HeadlessGhidraProjectManager extends DefaultProjectManager {}
}
