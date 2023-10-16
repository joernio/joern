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

  protected def astForMethod(methodDeclaration: SootMethod, typeDecl: RefType, childNum: Int): Ast = {
    val methodNode = createMethodNode(methodDeclaration, typeDecl, childNum)
    try {
      if (!methodDeclaration.isConcrete) {
        // Soot is not able to parse origin parameter names of abstract methods
        // https://github.com/soot-oss/soot/issues/1517
        val locals = methodDeclaration.getParameterTypes.asScala.zipWithIndex
          .map { case (typ, index) => new JimpleLocal(s"param${index + 1}", typ) }
        val parameterAsts =
          Seq(createThisNode(methodDeclaration, NewMethodParameterIn())) ++
            withOrder(locals) { (p, order) => astForParameter(p, order, methodDeclaration, Map()) }
        Ast(methodNode)
          .withChildren(astsForModifiers(methodDeclaration))
          .withChildren(parameterAsts)
          .withChildren(astsForHostTags(methodDeclaration))
          .withChild(Ast(NewBlock()))
          .withChild(astForMethodReturn(methodDeclaration))
      } else {
        val lastOrder = 2 + methodDeclaration.getParameterCount
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
          Seq(createThisNode(methodDeclaration, NewMethodParameterIn())) ++ withOrder(methodBody.getParameterLocals) {
            (p, order) => astForParameter(p, order, methodDeclaration, parameterAnnotations)
          }
        Ast(
          methodNode
            .lineNumberEnd(methodBody.toString.split('\n').filterNot(_.isBlank).length)
            .code(methodBody.toString)
        )
          .withChildren(astsForModifiers(methodDeclaration))
          .withChildren(parameterAsts)
          .withChildren(astsForHostTags(methodDeclaration))
          .withChild(astForMethodBody(methodBody, lastOrder))
          .withChild(astForMethodReturn(methodDeclaration))
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
        Ast(methodNode)
          .withChildren(astsForModifiers(methodDeclaration))
          .withChildren(astsForHostTags(methodDeclaration))
          .withChild(astForMethodReturn(methodDeclaration))
    } finally {
      // Join all targets with CFG edges - this seems to work from what is seen on DotFiles
      controlTargets.foreach({ case (asts, units) =>
        asts.headOption match {
          case Some(value) =>
            diffGraph.addEdge(value.root.get, unitToAsts(units).last.root.get, EdgeTypes.CFG)
          case None =>
        }
      })
      // Clear these maps
      controlTargets.clear()
      unitToAsts.clear()
    }
  }

  private def astsForModifiers(methodDeclaration: SootMethod): Seq[Ast] = {
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
    ).flatten.map { modifier =>
      Ast(NewModifier().modifierType(modifier).code(modifier.toLowerCase))
    }
  }

  private def astForMethodReturn(methodDeclaration: SootMethod): Ast = {
    val typeFullName = registerType(methodDeclaration.getReturnType.toQuotedString)
    val methodReturnNode = NodeBuilders
      .newMethodReturnNode(typeFullName, None, line(methodDeclaration), None)
      .order(methodDeclaration.getParameterCount + 2)
    Ast(methodReturnNode)
  }

  private def createMethodNode(methodDeclaration: SootMethod, typeDecl: RefType, childNum: Int) = {
    val fullName       = methodFullName(typeDecl, methodDeclaration)
    val methodDeclType = registerType(methodDeclaration.getReturnType.toQuotedString)
    val code = if (!methodDeclaration.isConstructor) {
      s"$methodDeclType ${methodDeclaration.getName}${paramListSignature(methodDeclaration, withParams = true)}"
    } else {
      s"${typeDecl.getClassName}${paramListSignature(methodDeclaration, withParams = true)}"
    }
    NewMethod()
      .name(methodDeclaration.getName)
      .fullName(fullName)
      .code(code)
      .signature(methodDeclType + paramListSignature(methodDeclaration))
      .isExternal(false)
      .order(childNum)
      .filename(filename)
      .astParentType(NodeTypes.TYPE_DECL)
      .astParentFullName(typeDecl.toQuotedString)
      .lineNumber(line(methodDeclaration))
      .columnNumber(column(methodDeclaration))
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

  protected def createParameterNode(parameterRef: ParameterRef, order: Int): Ast = {
    val name = s"@parameter${parameterRef.getIndex}"
    Ast(
      NewIdentifier()
        .name(name)
        .code(name)
        .typeFullName(registerType(parameterRef.getType.toQuotedString))
        .order(order)
        .argumentIndex(order)
    )
  }

  private def astForParameter(
    parameter: soot.Local,
    childNum: Int,
    methodDeclaration: SootMethod,
    parameterAnnotations: Map[String, VisibilityAnnotationTag]
  ): Ast = {
    val typeFullName = registerType(parameter.getType.toQuotedString)

    val parameterNode = Ast(
      NewMethodParameterIn()
        .name(parameter.getName)
        .code(s"$typeFullName ${parameter.getName}")
        .typeFullName(typeFullName)
        .order(childNum)
        .index(childNum)
        .lineNumber(line(methodDeclaration))
        .columnNumber(column(methodDeclaration))
        .evaluationStrategy(getEvaluationStrategy(parameter.getType))
    )

    parameterAnnotations.get(parameter.getName) match {
      case Some(annoRoot) =>
        parameterNode.withChildren(withOrder(annoRoot.getAnnotations.asScala) { (a, order) =>
          astsForAnnotations(a, order, methodDeclaration)
        })
      case None => parameterNode
    }
  }

  private def astForMethodBody(body: Body, order: Int): Ast = {
    val block        = NewBlock().order(order).lineNumber(line(body)).columnNumber(column(body))
    val jimpleParams = body.getParameterLocals.asScala.toList
    // Don't let parameters also become locals (avoiding duplication)
    val jimpleLocals = body.getLocals.asScala.filterNot(l => jimpleParams.contains(l) || l.getName == "this").toList
    val locals = withOrder(jimpleLocals) { case (l, order) =>
      val name         = l.getName
      val typeFullName = registerType(l.getType.toQuotedString)
      val code         = s"$typeFullName $name"
      Ast(NewLocal().name(name).code(code).typeFullName(typeFullName).order(order))
    }
    Ast(block)
      .withChildren(locals)
      .withChildren(withOrder(body.getUnits.asScala.filterNot(isIgnoredUnit)) { (x, order) =>
        astsForStatement(x, order + locals.size)
      }.flatten)
  }

}
