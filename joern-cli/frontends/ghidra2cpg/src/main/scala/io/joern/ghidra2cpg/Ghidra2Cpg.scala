package io.joern.ghidra2cpg

import ghidra.GhidraJarApplicationLayout
import ghidra.base.project.GhidraProject
import ghidra.framework.protocol.ghidra.Handler
import ghidra.framework.{Application, HeadlessGhidraApplicationConfiguration}
import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.model.listing.Program
import ghidra.program.util.{DefinedDataIterator, GhidraProgramUtilities}
import ghidra.util.exception.InvalidInputException
import io.joern.ghidra2cpg.passes.*
import io.joern.ghidra2cpg.passes.arm.ArmFunctionPass
import io.joern.ghidra2cpg.passes.mips.{LoHiPass, MipsFunctionPass}
import io.joern.ghidra2cpg.passes.x86.{ReturnEdgesPass, X86FunctionPass}
import io.joern.ghidra2cpg.utils.{CommandLineConfig, Decompiler}
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{X2Cpg, X2CpgFrontend}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

class Ghidra2Cpg extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig: Config = Config()

  /** Create a CPG representing the given input file. The CPG is stored at the given output file. The caller must close
    * the CPG.
    */
  override def createCpg(config: Config): Try[Cpg] = {
    val inputFile = new File(config.inputPath)
    if (!inputFile.isDirectory && !inputFile.isFile) {
      throw new InvalidInputException(s"$inputFile is not a valid directory or file.")
    }

    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, _) =>
      FileUtil.usingTemporaryDirectory("ghidra2cpg_tmp") { tempWorkingDir =>
        initGhidra()
        var program: Program       = null
        var project: GhidraProject = null;

        try {
          // The 'true' parameter indicates this is a temporary project
          project =
            GhidraProject.createProject(tempWorkingDir.absolutePathAsString, CommandLineConfig.projectName, true)
          program = project.importProgram(inputFile)
          addProgramToCpg(program, inputFile.getCanonicalPath, cpg)
        } catch {
          case e: Exception =>
            e.printStackTrace()
        } finally {
          // Closing deletes the project (since we created a temporary project)
          // Closing also releases the program
          project.close()
        }
      }
    }
  }

  private def initGhidra(): Unit = {
    // We need this for the URL handler
    Handler.registerHandler()

    // Initialize application (if necessary and only once)
    if (!Application.isInitialized) {
      val configuration = new HeadlessGhidraApplicationConfiguration
      configuration.setInitializeLogging(false)
      Application.initializeApplication(new GhidraJarApplicationLayout, configuration)
    }
  }

  private def addProgramToCpg(program: Program, fileAbsolutePath: String, cpg: Cpg): Unit = {
    val transactionId: Int = program.startTransaction("Analysis")
    try {
      GhidraProject.analyze(program);
      GhidraProgramUtilities.markProgramAnalyzed(program)
      handleProgram(program, fileAbsolutePath, cpg)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    } finally {
      program.endTransaction(transactionId, true)
    }

  }

  def handleProgram(program: Program, fileAbsolutePath: String, cpg: Cpg): Unit = {

    val flatProgramAPI: FlatProgramAPI = new FlatProgramAPI(program)
    val decompiler                     = Decompiler(program).get

    // Functions
    val listing          = program.getListing
    val functionIterator = listing.getFunctions(true)
    val functions        = functionIterator.iterator.asScala.toList

    val address2Literals: Map[Long, String] = DefinedDataIterator
      .definedStrings(program)
      .iterator()
      .asScala
      .toList
      .map(x => x.getAddress().getOffset -> x.getValue.toString)
      .toMap

    new MetaDataPass(cpg, Languages.GHIDRA, fileAbsolutePath).createAndApply()
    Option(flatProgramAPI.getProgramFile).foreach { programFile =>
      new NamespacePass(cpg, programFile).createAndApply()
    }

    program.getLanguage.getLanguageDescription.getProcessor.toString match {
      case "MIPS" =>
        new MipsFunctionPass(program, address2Literals, fileAbsolutePath, functions, cpg, decompiler).createAndApply()
        new LoHiPass(cpg).createAndApply()
      case "AARCH64" | "ARM" =>
        new ArmFunctionPass(program, fileAbsolutePath, functions, cpg, decompiler)
          .createAndApply()
      case _ =>
        new X86FunctionPass(program, fileAbsolutePath, functions, cpg, decompiler)
          .createAndApply()
        new ReturnEdgesPass(cpg).createAndApply()
    }

    TypeNodePass.withRegisteredTypes(Types.types.toList, cpg).createAndApply()
    new JumpPass(cpg).createAndApply()
    new LiteralPass(cpg, flatProgramAPI).createAndApply()
  }
}

object Types {
  // Types will be added to the CPG as soon as everything
  // else is done
  val types: mutable.SortedSet[String] = mutable.SortedSet[String]()
  def registerType(typeName: String): String = {
    try {
      types += typeName
    } catch {
      case _: Exception => println(s" Error adding type: $typeName")
    }
    typeName
  }
}
