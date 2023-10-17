package io.joern.jimple2cpg.astcreation.declarations

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory
import soot.jimple.*
import soot.jimple.internal.JimpleLocal
import soot.tagkit.*
import soot.{SootMethod, Local as _, *}

import scala.collection.immutable.HashSet
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

trait AstForMethodsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(getClass)

  // There are many, but the popular ones should do https://en.wikipedia.org/wiki/List_of_JVM_languages
  private val JVM_LANGS = HashSet("scala", "clojure", "groovy", "kotlin", "jython", "jruby")

  protected def astForMethod(methodDeclaration: SootMethod, typeDecl: RefType): Ast = {
    val methodNode = createMethodNode(methodDeclaration, typeDecl)
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
              astForParameter(param, index, methodDeclaration, parameterAnnotations)
            }

        methodAstWithAnnotations(
          methodNode
            .lineNumberEnd(methodBody.toString.split('\n').filterNot(_.isBlank).length)
            .code(methodBody.toString),
          parameterAsts,
          astForMethodBody(methodBody),
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
            s"'${methodNode.fullName}' correctly."
          logger.warn(
            if (nonJavaLibs.nonEmpty) s"$errMsg. Language(s) detected: ${nonJavaLibs.mkString(",")}."
            else errMsg
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
      controlTargets.foreach { case (asts, units) =>
        asts.headOption match {
          case Some(value) =>
            diffGraph.addEdge(value.root.get, unitToAsts(units).last.root.get, EdgeTypes.CFG)
          case None =>
        }
      }
      // Clear these maps
      controlTargets.clear()
      unitToAsts.clear()
    }
  }

  private def astsForModifiers(methodDeclaration: SootMethod): Seq[NewModifier] = {
    Seq(
      if (methodDeclaration.isStatic) Some(ModifierTypes.STATIC) else None,
      if (methodDeclaration.isPublic) Some(ModifierTypes.PUBLIC) else None,
      if (methodDeclaration.isProtected) Some(ModifierTypes.PROTECTED) else None,
      if (methodDeclaration.isPrivate) Some(ModifierTypes.PRIVATE) else None,
      if (methodDeclaration.isAbstract) Some(ModifierTypes.ABSTRACT) else None,
      if (methodDeclaration.isConstructor) Some(ModifierTypes.CONSTRUCTOR) else None,
      if (!methodDeclaration.isFinal && !methodDeclaration.isStatic && methodDeclaration.isPublic)
        Some(ModifierTypes.VIRTUAL)
      else None,
      if (methodDeclaration.isSynchronized) Some("SYNCHRONIZED") else None
    ).flatten.map(NodeBuilders.newModifierNode(_).code(modifier.toLowerCase))
  }

  private def astForMethodReturn(methodDeclaration: SootMethod): Ast = {
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
      NodeTypes.TYPE_DECL,
      typeDecl.toQuotedString
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

  protected def astForParameterRef(parameterRef: ParameterRef): Ast = {
    val name = s"@parameter${parameterRef.getIndex}"
    Ast(identifierNode(parameterRef, name, name, registerType(parameterRef.getType.toQuotedString)))
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
        parameter,
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
        paramAst.withChildren(annotationAsts)
      case None => paramAst
    }
  }

  private def astForMethodBody(body: Body): Ast = {
    val jimpleParams = body.getParameterLocals.asScala.toList
    // Don't let parameters also become locals (avoiding duplication)
    val jimpleLocals = body.getLocals.asScala.filterNot(l => jimpleParams.contains(l) || l.getName == "this").toList
    val locals = jimpleLocals.map { local =>
      val name         = local.getName
      val typeFullName = registerType(local.getType.toQuotedString)
      val code         = s"$typeFullName $name"
      localNode(local, name, code, typeFullName)
    }
    val statements = body.getUnits.asScala.filterNot(isIgnoredUnit).flatMap(astsForStatement).toSeq
    blockAst(blockNode(body), locals ++ statements)
  }

}
