package io.joern.jimple2cpg.astcreation.declarations

import cats.syntax.all.*
import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.jimple2cpg.astcreation.statements.BodyControlInfo
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory
import soot.jimple.*
import soot.jimple.internal.JimpleLocal
import soot.tagkit.*
import soot.{SootMethod, Local as SootLocal, Unit as SUnit, *}

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

trait AstForMethodsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(getClass)

  // There are many, but the popular ones should do https://en.wikipedia.org/wiki/List_of_JVM_languages
  private val JVM_LANGS = HashSet("scala", "clojure", "groovy", "kotlin", "jython", "jruby")

  protected def astForMethod(methodDeclaration: SootMethod, typeDecl: RefType): Ast = {
    val bodyStatementsInfo = BodyControlInfo()
    val methodNode         = createMethodNode(methodDeclaration, typeDecl)
    try {
      if (!methodDeclaration.isConcrete) {
        // Soot is not able to parse origin parameter names of abstract methods
        // https://github.com/soot-oss/soot/issues/1517
        val locals = methodDeclaration.getParameterTypes.asScala.zipWithIndex
          .map { case (typ, index) => new JimpleLocal(s"param${index + 1}", typ) }
        val parameterAsts =
          Seq(createThisNode(methodDeclaration, NewMethodParameterIn())) ++
            locals.zipWithIndex.map { case (param, index) =>
              astForParameter(param, index + 1, methodDeclaration, Map())
            }

        methodAstWithAnnotations(
          methodNode,
          parameterAsts,
          Ast(NewBlock()),
          astForMethodReturn(methodDeclaration),
          astsForModifiers(methodDeclaration),
          astsForHostTags(methodDeclaration)
        )
      } else {
        // Map params to their annotations
        val mTags = methodDeclaration.getTags.asScala
        val paramAnnos =
          mTags.collect { case x: VisibilityParameterAnnotationTag => x }.flatMap(_.getVisibilityAnnotations.asScala)
        val paramNames           = mTags.collect { case x: ParamNamesTag => x }.flatMap(_.getNames.asScala)
        val parameterAnnotations = paramNames.zip(paramAnnos).filter(_._2 != null).toMap
        val methodBody = Try(methodDeclaration.getActiveBody) match {
          case Failure(_)    => methodDeclaration.retrieveActiveBody()
          case Success(body) => body
        }
        val parameterAsts =
          Seq(createThisNode(methodDeclaration, NewMethodParameterIn())) ++
            methodBody.getParameterLocals.asScala.zipWithIndex.map { case (param, index) =>
              astForParameter(param, index + 1, methodDeclaration, parameterAnnotations)
            }

        methodAstWithAnnotations(
          methodNode
            .lineNumberEnd(methodBody.toString.split('\n').filterNot(_.isBlank).length)
            .code(methodBody.toString),
          parameterAsts,
          astForMethodBody(methodBody, bodyStatementsInfo),
          astForMethodReturn(methodDeclaration),
          astsForModifiers(methodDeclaration),
          astsForHostTags(methodDeclaration)
        )
      }
    } catch {
      case e: RuntimeException =>
        // Use a few heuristics to determine if this is not built with the JDK
        val nonJavaLibs = cls.getInterfaces.asScala.map(_.getPackageName).filter(JVM_LANGS.contains).toSet
        if (nonJavaLibs.nonEmpty || cls.getMethods.asScala.exists(_.getName.endsWith("$"))) {
          val errMsg = "The bytecode for this method suggests it is built with a non-Java JVM language. " +
            "Soot requires including the specific language's SDK in the analysis to create the method body for " +
            s"'${methodNode.fullName}' correctly. This detection warning will only be emitted once."
          AstForMethodsCreator.emitNotJvmLanguageDetected(() =>
            logger.warn(
              if (nonJavaLibs.nonEmpty) s"$errMsg. Language(s) detected: ${nonJavaLibs.mkString(",")}."
              else errMsg
            )
          )
        } else {
          logger.warn(
            s"Unexpected runtime exception while parsing method body! Will stub the method '${methodNode.fullName}''",
            e
          )
        }
        methodAstWithAnnotations(
          methodNode,
          Seq.empty,
          Ast(NewBlock()),
          astForMethodReturn(methodDeclaration),
          astsForModifiers(methodDeclaration),
          astsForHostTags(methodDeclaration)
        )
    } finally {
      // Join all targets with CFG edges - this seems to work from what is seen on DotFiles
      bodyStatementsInfo.targets.foreach { case (asts, unit) =>
        asts.headOption match {
          case Some(value) =>
            bodyStatementsInfo.unitToAsts.get(unit) match {
              case Some(targetAsts) if targetAsts.nonEmpty =>
                diffGraph.addEdge(value.root.get, targetAsts.last.root.get, EdgeTypes.CFG)
              case _ =>
                logger.error(
                  s"AstForMethodsCreator: Missing unit in unitToAsts: $unit (${unit.getClass.getSimpleName})"
                )
            }
          case None =>
            logger.error("AstForMethodsCreator: Empty asts list for target")
        }
      }

      bodyStatementsInfo.edges.foreach { case (a, b) =>
        (bodyStatementsInfo.unitToAsts.get(a), bodyStatementsInfo.unitToAsts.get(b)) match {
          case (Some(aAsts), Some(bAsts)) if aAsts.nonEmpty && bAsts.nonEmpty =>
            val aNode = aAsts.last.root.get
            val bNode = bAsts.last.root.get
            diffGraph.addEdge(aNode, bNode, EdgeTypes.CFG)
          case _ =>
            logger.error(
              s"AstForMethodsCreator: Failed to add CFG edge between units: " +
                s"a=${a.getClass.getSimpleName} (${a.toString.take(50)}) " +
                s"b=${b.getClass.getSimpleName} (${b.toString.take(50)})"
            )
            if (bodyStatementsInfo.unitToAsts.get(a).isEmpty) {
              logger.debug(s"AstForMethodsCreator: Missing source unit in unitToAsts: $a (${a.getClass.getSimpleName})")
            }
            if (bodyStatementsInfo.unitToAsts.get(b).isEmpty) {
              logger.debug(s"AstForMethodsCreator: Missing target unit in unitToAsts: $b (${b.getClass.getSimpleName})")
            }
        }
      }
    }
  }

  private def astForMethodReturn(methodDeclaration: SootMethod): NewMethodReturn = {
    val typeFullName = registerType(methodDeclaration.getReturnType.toQuotedString)
    methodReturnNode(methodDeclaration, typeFullName)
  }

  private def createMethodNode(methodDeclaration: SootMethod, typeDecl: RefType) = {
    val name           = methodDeclaration.getName
    val fullName       = methodFullName(typeDecl, methodDeclaration)
    val methodDeclType = registerType(methodDeclaration.getReturnType.toQuotedString)
    val code = if (!methodDeclaration.isConstructor) {
      s"$methodDeclType $name${paramListSignature(methodDeclaration, withParams = true)}"
    } else {
      s"${typeDecl.getClassName}${paramListSignature(methodDeclaration, withParams = true)}"
    }
    val signature = s"$methodDeclType${paramListSignature(methodDeclaration)}"
    methodNode(
      methodDeclaration,
      name,
      code,
      fullName,
      Option(signature),
      filename,
      Option(NodeTypes.TYPE_DECL),
      Option(typeDecl.toQuotedString)
    )
  }

  private def methodFullName(typeDecl: RefType, methodDeclaration: SootMethod): String = {
    val typeName   = registerType(typeDecl.toQuotedString)
    val returnType = registerType(methodDeclaration.getReturnType.toQuotedString)
    val methodName = methodDeclaration.getName
    s"$typeName.$methodName:$returnType${paramListSignature(methodDeclaration)}"
  }

  private def paramListSignature(methodDeclaration: SootMethod, withParams: Boolean = false) = {
    val paramTypes = methodDeclaration.getParameterTypes.asScala.map(x => registerType(x.toQuotedString))

    val paramNames =
      if (!methodDeclaration.isPhantom && Try(methodDeclaration.retrieveActiveBody()).isSuccess)
        methodDeclaration.retrieveActiveBody().getParameterLocals.asScala.map(_.getName)
      else
        paramTypes.zipWithIndex.map(x => {
          s"param${x._2 + 1}"
        })
    if (!withParams) {
      "(" + paramTypes.mkString(",") + ")"
    } else {
      "(" + paramTypes.zip(paramNames).map(x => s"${x._1} ${x._2}").mkString(", ") + ")"
    }
  }

  protected def astForParameterRef(parameterRef: ParameterRef, parentUnit: SUnit): Ast = {
    val name = s"@parameter${parameterRef.getIndex}"
    Ast(identifierNode(parentUnit, name, name, registerType(parameterRef.getType.toQuotedString)))
  }

  private def astForParameter(
    parameter: soot.Local,
    index: Int,
    methodDeclaration: SootMethod,
    parameterAnnotations: Map[String, VisibilityAnnotationTag]
  ): Ast = {
    val typeFullName = registerType(parameter.getType.toQuotedString)

    val paramAst = Ast(
      parameterInNode(
        methodDeclaration,
        parameter.getName,
        s"$typeFullName ${parameter.getName}",
        index,
        isVariadic = false, // Variadic types are converted to explicit arrays in bytecode
        getEvaluationStrategy(parameter.getType),
        Option(typeFullName)
      )
    )

    parameterAnnotations.get(parameter.getName) match {
      case Some(annoRoot) =>
        val annotationAsts = annoRoot.getAnnotations.asScala.map(astsForAnnotations(_, methodDeclaration)).toSeq
        paramAst
          .withChildren(annotationAsts)
      case None => paramAst
    }
  }

  private def astForMethodBody(body: Body, info: BodyControlInfo): Ast = {
    val jimpleParams = body.getParameterLocals.asScala.toList
    // Don't let parameters also become locals (avoiding duplication)
    val jimpleLocals = body.getLocals.asScala.filterNot(l => jimpleParams.contains(l) || l.getName == "this").toList
    val locals = jimpleLocals.map { local =>
      val name         = local.getName
      val typeFullName = registerType(local.getType.toQuotedString)
      val code         = s"$typeFullName $name"
      Ast(localNode(body, name, code, typeFullName))
    }

    // Indicate trap boundaries
    val pushTraps: mutable.HashMap[SUnit, List[Trap]] = mutable.HashMap.empty
    val popTraps: mutable.HashMap[SUnit, Int]         = mutable.HashMap.empty
    body.getTraps.asScala.toList.reverseIterator.foreach { trap =>
      pushTraps.updateWith(trap.getBeginUnit)(Option(List(trap)) combine _)
      popTraps.updateWith(trap.getEndUnit)(Option(1) combine _)
    }

    stack.push(Ast(blockNode(body)).withChildren(locals))

    val trapStack = new mutable.Stack[soot.Trap];
    body.getUnits.asScala.filterNot(isIgnoredUnit).foreach { statement =>
      // Remove traps that ended on the previous unit
      (1 to popTraps.getOrElse(statement, 0)).foreach { _ =>
        if (trapStack.nonEmpty) {
          trapStack.pop()
        }
      }

      // Add traps that apply to this unit
      pushTraps.getOrElse(statement, List.empty).foreach(trapStack.push)

      val asts = astsForStatement(statement, info)

      // Add a control edge to the handler for each trap applying to this unit
      trapStack.foreach { trap =>
        val handler = trap.getHandlerUnit();
        info.edges.addOne(statement -> handler)
      }

      stack.push(stack.pop().withChildren(asts))
    }

    if (stack.nonEmpty) {
      stack.pop()
    } else {
      Ast(blockNode(body))
    }
  }

}

object AstForMethodsCreator {

  private val nonJvmWarnEmitted = AtomicBoolean(false)

  private def emitNotJvmLanguageDetected(logFunction: () => Unit): Unit = {
    if (!nonJvmWarnEmitted.getAndSet(true)) {
      logFunction()
    }
  }
}
