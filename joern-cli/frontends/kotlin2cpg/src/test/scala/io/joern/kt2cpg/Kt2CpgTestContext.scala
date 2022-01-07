package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.types.{CompilerAPI, InferenceSourcesPicker, KotlinTypeInfoProvider, TypeInfoProvider}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.{Base, CallGraph, ControlFlow, LayerCreatorContext, TypeRelations}
import org.jetbrains.kotlin.psi.{
  KtBinaryExpression,
  KtCallExpression,
  KtClass,
  KtClassOrObject,
  KtDotQualifiedExpression,
  KtElement,
  KtExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtObjectDeclaration,
  KtParameter,
  KtProperty,
  KtQualifiedExpression,
  KtTypeAlias,
  KtValueArgument
}
import better.files._

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

class EmptyTypeInfoProvider extends TypeInfoProvider {
  def containingDeclType(expr: KtQualifiedExpression, or: String): String = {
    "EMPTY_TYPE"
  }

  def returnType(expr: KtNamedFunction, or: String): String = {
    "EMPTY_TYPE"
  }

  def expressionType(expr: KtExpression, or: String): String = {
    "EMPTY_TYPE"
  }

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String] = {
    Seq("EMPTY_TYPE")
  }

  def parameterType(expr: KtParameter, or: String): String = {
    "EMPTY_TYPE"
  }

  def propertyType(expr: KtProperty, or: String): String = {
    "EMPTY_TYPE"
  }

  def fullName(expr: KtClassOrObject, or: String): String = {
    "EMPTY"
  }

  def fullNameWithSignature(expr: KtQualifiedExpression, or: (String, String)): (String, String) = {
    ("METHOD_FULL_NAME", "SIGNATURE")
  }

  def fullNameWithSignature(expr: KtCallExpression, or: (String, String)): (String, String) = {
    ("METHOD_FULL_NAME", "SIGNATURE")
  }

  def fullNameWithSignature(call: KtBinaryExpression, or: (String, String)): (String, String) = {
    ("METHOD_FULL_NAME", "SIGNATURE")
  }

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String) = {
    ("METHOD_FULL_NAME", "SIGNATURE")
  }

  def fullName(expr: KtTypeAlias, or: String): String = {
    "FULL_NAME"
  }

  def aliasTypeFullName(expr: KtTypeAlias, or: String): String = {
    "ALIAS_TYPE_FULL_NAME"
  }

  def typeFullName(expr: KtNameReferenceExpression, or: String): String = {
    "TYPE_FULL_NAME"
  }
}

object Kt2CpgTestContext {
  def newContext: Kt2CpgTestContext = {
    new Kt2CpgTestContext()
  }

  /** This is a shorthand for newContext.addSource().buildCpg
    */
  def buildCpg(code: String, file: String = "test.kt"): Cpg = {
    val context = new Kt2CpgTestContext()
    context.addSource(code, file)
    context.buildCpg
  }
}

class Kt2CpgTestContext private () {
  private val codeAndFile = mutable.ArrayBuffer.empty[Kt2Cpg.InputPair]
  private var buildResult = Option.empty[Cpg]

  def addSource(code: String, fileName: String = "test.kt"): Kt2CpgTestContext = {
    if (buildResult.nonEmpty) {
      throw new RuntimeException("Not allowed to add sources after buildCpg() was called.")
    }
    if (codeAndFile.exists(_.fileName == fileName)) {
      throw new RuntimeException(s"Add more than one source under file path $fileName.")
    }
    codeAndFile.append(Kt2Cpg.InputPair(code, fileName))
    this
  }

  def buildCpg: Cpg = {
    if (buildResult.isEmpty) {
      val tempDir = File.newTemporaryDirectory().deleteOnExit(true)
      codeAndFile.foreach { inputPair =>
        val file = tempDir / inputPair.fileName
        file.writeText(inputPair.content)
      }

      // TODO: iterate over inferencejars dir and get the paths like so

      val inferenceJarDir = File("src/main/resources/inferencejars")
      val inferenceJarsPaths =
        inferenceJarDir.list
          .filter(_.hasExtension)
          .filter(_.pathAsString.endsWith("jar"))
          .map { f =>
            InferenceJarPath(f.pathAsString, false)
          }
          .toSeq

      val environment = CompilerAPI.makeEnvironment(Seq(tempDir.pathAsString), inferenceJarsPaths)
      val filesWithMeta =
        environment.getSourceFiles.asScala
          .map { fm =>
            KtFileWithMeta(fm, "GENERATED_PLACEHOLDER_FILE.kt", fm.getVirtualFilePath)
          }
      val typeInfoProvider = new KotlinTypeInfoProvider(environment)
      val kt2Cpg = new Kt2Cpg()
      val cpg = kt2Cpg.createCpg(filesWithMeta, codeAndFile.map(inputPair => () => inputPair), typeInfoProvider)
      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
      new TypeRelations().run(context)
      new ControlFlow().run(context)
      new CallGraph().run(context)
      buildResult = Some(cpg)
    }
    buildResult.get
  }
}
