package io.joern.dataflowengineoss.slicing

import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Languages, Operators, Properties}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Paths}
import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicBoolean
import java.util.regex.Pattern
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

/** A utility for slicing based off of usage references for identifiers and parameters. This is mainly tested around
  * JavaScript CPGs.
  */
object UsageSlicing {

  private val logger                 = LoggerFactory.getLogger(getClass)
  private val resolver               = NoResolve
  private val constructorTypeMatcher = Pattern.compile(".*new (\\w+)\\(.*")
  private val excludeOperatorCalls   = new AtomicBoolean(false)

  /** Generates object slices from the given CPG.
    *
    * @param cpg
    *   the CPG to slice.
    * @return
    *   a set of object slices.
    */
  def calculateUsageSlice(cpg: Cpg, config: UsagesConfig): ProgramUsageSlice = {
    implicit val implicitConfig: UsagesConfig = config
    excludeOperatorCalls.set(config.excludeOperatorCalls)

    lazy val methods = (config.fileFilter match {
      case Some(fileName) => cpg.file.nameExact(fileName).method
      case None           => cpg.method
    }).withMethodNameFilter.withMethodParameterFilter.withMethodAnnotationFilter.toList

    lazy val declarations = methods.iterator.declaration.toList

    // The JavaScript frontends define fields (also those with an initializer) by assigning `this.<field>`.
    // TODO: this should actually hold for most frontends - we should revisit this.
    lazy val fields =
      if (cpg.metaData.language.exists(lang => lang == Languages.JSSRC || lang == Languages.JAVASCRIPT))
        methods.flatMap(fieldDefinitions)
      else Nil

    def typeMap = TrieMap.from(cpg.typeDecl.map(f => (f.name, f.fullName)).toMap)

    val slices       = usageSlices(cpg, declarations, fields, typeMap)
    val userDefTypes = userDefinedTypes(cpg)
    ProgramUsageSlice(slices, userDefTypes)
  }

  import io.shiftleft.semanticcpg.codedumper.CodeDumper.dump

  private def usageSlices(
    cpg: Cpg,
    declarations: List[Declaration],
    fields: List[FieldDefinition],
    typeMap: TrieMap[String, String]
  )(implicit config: UsagesConfig): List[MethodUsageSlice] = {
    val language         = cpg.metaData.language.headOption
    val root             = cpg.metaData.root.headOption
    val declarationTasks = declarations
      .filter(decl => atLeastNCalls(decl, config.minNumCalls) && !decl.name.startsWith("_tmp_"))
      .map(decl => () => new TrackUsageTask(cpg, decl, None, typeMap).call())
    val fieldTasks = fields
      .filter(field => getInCallsForField(field).sizeIs >= config.minNumCalls)
      .map(field => () => new TrackUsageTask(cpg, field.member, Option(field), typeMap).call())
    val tasks = (declarationTasks ++ fieldTasks).iterator
    ConcurrentTaskUtil
      .runUsingThreadPool(tasks, config.parallelism.getOrElse(Runtime.getRuntime.availableProcessors()))
      .flatMap {
        case Success(slice) => slice
        case Failure(e)     =>
          logger.warn("Exception encountered during slicing task", e)
          None
      }
      .groupBy { case (scope, _) => scope }
      .view
      .sortBy(_._1.fullName)
      .map { case (method, slices) =>
        MethodUsageSlice(
          code =
            if (config.excludeMethodSource || !Files.exists(Paths.get(method.filename))) ""
            else Try(dump(method.location, language, root, highlight = false, withArrow = false)).getOrElse(""),
          fullName = method.fullName,
          fileName = method.filename,
          slices = slices.iterator.map(_._2).toSet,
          lineNumber = method.lineNumber.map(_.intValue()),
          columnNumber = method.columnNumber.map(_.intValue())
        )
      }
      .toList
  }

  private class TrackUsageTask(
    cpg: Cpg,
    tgt: Declaration,
    field: Option[FieldDefinition],
    typeMap: TrieMap[String, String]
  ) extends Callable[Option[(Method, ObjectUsageSlice)]] {

    @scala.annotation.nowarn("cat=deprecation")
    override def call(): Option[(Method, ObjectUsageSlice)] = {
      val defNode = tgt match {
        case _: Member =>
          field.flatMap(fieldDef => fieldDef.assignment.argument.argumentIndex(2).headOption).map {
            // In the case of a constructor, we should get the "new" call
            case block: Block => block.ast.isCall.nameExact("<operator>.new").lastOption.getOrElse(block)
            case other        => other
          }
        case local: Local =>
          local.referencingIdentifiers.inCall.astParent.assignment
            .where(_.argument(1).code(tgt.name))
            .argument(2)
            .headOption match {
            // In the case of a constructor, we should get the "new" call
            case Some(block: Block) =>
              block.ast.isCall.nameExact("<operator>.new").lastOption
            case x => x
          }
        case x => Some(x)
      }

      (tgt, defNode, partitionInvolvementInCalls) match {
        // Case 1a: Generated by assignment to a field
        case (member: Member, Some(genCall: Call), (invokedCalls, argToCalls))
            if !genCall.name.matches("(require|import)") =>
          field.map { fieldDef =>
            val definedBy = createDefComponent(genCall)
            (
              fieldDef.method,
              ObjectUsageSlice(
                targetObj = createDefComponent(fieldDef.access, member.name, definedBy),
                definedBy = Option(definedBy),
                invokedCalls = invokedCalls,
                argToCalls = argToCalls
              )
            )
          }
        // Case 1: Generated by variable assignment
        case (local: Local, Some(genCall: Call), (invokedCalls, argToCalls))
            if !genCall.name.matches("(require|import)") =>
          Option(
            (
              local.method.head,
              ObjectUsageSlice(
                targetObj = createDefComponent(local),
                definedBy = Option(createDefComponent(genCall)),
                invokedCalls = invokedCalls,
                argToCalls = argToCalls
              )
            )
          )
        // Case 2: Generated by incoming parameter
        case (param: MethodParameterIn, _, (invokedCalls, argToCalls)) if !param.name.matches("(this|self)") =>
          Option(
            (
              param.method,
              ObjectUsageSlice(
                targetObj = createDefComponent(param),
                definedBy = Option(createDefComponent(param)),
                invokedCalls = invokedCalls,
                argToCalls = argToCalls
              )
            )
          )
        case _ => None
      }
    }

    /** Will attempt to get the API call from the expression if this is a procedure call.
      *
      * @param baseCall
      *   the expression to extract the API call from.
      * @return
      *   an API call if present.
      */
    private def exprToObservedCall(baseCall: Call): Option[ObservedCall] = {
      val isMemberInvocation = baseCall.name.equals(Operators.fieldAccess)
      val isConstructor      =
        baseCall.name.equals(Operators.alloc) || baseCall.ast.isCall.nameExact(Operators.alloc).nonEmpty

      def getResolvedMethod(x: Call): Option[String] = if (
        DefComponent.unresolvedCallPattern.matcher(x.methodFullName).matches()
      ) None
      else Option(x.methodFullName)

      // Handle the case where a call is an invocation of a field member (lambda) or function/method call
      val callNameAndResolvedMethod: (Option[String], Option[String]) =
        if (isMemberInvocation)
          baseCall.argumentOut
            .flatMap {
              case x: FieldIdentifier =>
                Option(Option(x.code) -> None)
              case x: Call => Option(Option(x.name) -> getResolvedMethod(x))
              case _       => None
            }
            .headOption
            .getOrElse((None, None))
        else if (isConstructor) {
          val matcher  = constructorTypeMatcher.matcher(baseCall.code)
          val typeName =
            if (matcher.find()) matcher.group(1)
            else baseCall.code.stripPrefix("new ").takeWhile(!_.equals('('))
          Option(typeName) -> typeMap.get(typeName)
        } else
          Option(baseCall.name) -> getResolvedMethod(baseCall)
      val (callName, resolvedMethod) = callNameAndResolvedMethod

      if (callName.isEmpty) return None

      val params = (if (isMemberInvocation) baseCall.inCall.argument
                    else if (isConstructor)
                      baseCall.ast.isCall
                        .nameExact("<operator>.new")
                        .lastOption
                        .map(_.argument)
                        .getOrElse(Iterator.empty)
                    else baseCall.argument)
        .collect { case n: Expression if n.argumentIndex > 0 => n }
        .map {
          case _: MethodRef => "LAMBDA"
          case other        =>
            other
              .propertyOption(Properties.TypeFullName)
              .orElse(other.property(Properties.DynamicTypeHintFullName).headOption)
              .getOrElse("ANY")
        }
        .collect { case x: String => x }
        .toList
      // Not sure how we can get the return type unless it's typescript or we can resolve the callee?
      val returnType = if (isConstructor) {
        resolvedMethod match {
          case Some(methodFullName) => methodFullName
          case None                 => "ANY"
        }
      } else {
        baseCall.argumentOut
          .flatMap {
            case x: Call if !DefComponent.unresolvedCallPattern.matcher(x.methodFullName).matches() =>
              cpg.method.fullNameExact(x.methodFullName).methodReturn.typeFullName.headOption
            case x: Call =>
              x.callee(resolver).methodReturn.typeFullName.headOption
            case _ => None
          }
          .headOption
          .getOrElse("ANY")
      }

      Option(
        ObservedCall(
          callName.get,
          resolvedMethod,
          params,
          returnType,
          baseCall.lineNumber.map(_.intValue()),
          baseCall.columnNumber.map(_.intValue())
        )
      )
    }

    private def partitionInvolvementInCalls: (List[ObservedCall], List[ObservedCallWithArgPos]) = {
      val (invokedCalls, argToCalls) = field
        .map(getInCallsForField)
        .getOrElse(getInCallsForReferencedIdentifiers(tgt))
        .sortBy(call => (call.lineNumber, call.columnNumber))
        .flatMap(call =>
          call.argument
            .find(arg => field.map(isAccessOf(_, arg)).getOrElse(arg.code == tgt.name))
            .map(arg => (call, arg))
            .headOption
        )
        .map { case (c, arg) =>
          if (arg.argumentName.isDefined) (c, arg, Left(arg.argumentName.get))
          else (c, arg, Right(arg.argumentIndex))
        }
        .partition { case (c, _, argIdx) =>
          argIdx match {
            case Left(_)                     => false // receivers/bases are never named
            case Right(_) if c.isConstructor => true
            case Right(_) if c.isOperator    => false
            case Right(argIdx)               => argIdx == 0
          }
        }
      (
        invokedCalls.map(_._1).isCall.flatMap(exprToObservedCall).toList,
        argToCalls
          .flatMap { case (c: Call, _, argAt: Either[String, Int]) =>
            exprToObservedCall(c).map(oc => ObservedCallWithArgPos.fromObservedCall(oc, argAt))
          }
      )
    }

    /** Creates a def component with the workaround of correcting the type full name if it is only a type name.
      */
    private def createDefComponent(node: AstNode) = DefComponent.fromNode(node, typeMap.toMap)

    /** The field is not a node of its own in the method, so the access defining it stands in for it. If the access is
      * not typed, the field gets the type of what it is defined by.
      */
    private def createDefComponent(access: Call, name: String, definedBy: DefComponent): DefComponent = {
      val definedByType = definedBy match {
        case CallDef(_, typeFullName, _, _, _, _) => Option(typeFullName)
        case _                                    => None
      }
      createDefComponent(access) match {
        case CallDef(_, typeFullName, _, line, column, _) =>
          val accessType = Option(typeFullName).filterNot(_.matches("(ANY|UNKNOWN)"))
          LocalDef(name, accessType.orElse(definedByType).getOrElse(typeFullName), line, column)
        case other => other
      }
    }

    // TODO: Slicing may run before post-processing so we cannot assume a call graph
    //  implement this in a next step to combine slices
    private def linkSlices(slices: Map[String, Set[ObjectUsageSlice]]): Unit = {
      slices.foreach { case (_, usageSlices) =>
        usageSlices.foreach { slice =>
          slice.definedBy match {
            case Some(CallDef(_, _, Some(resolvedMethod), _, _, _)) =>
              slices.get(resolvedMethod) match {
                case Some(_) => // TODO: Handle match
                case None    => // No match
              }
            case _ =>
          }
          slice.argToCalls
            .flatMap {
              case ObservedCallWithArgPos(_, Some(resolvedMethod), _, _, Left(argName), _, _) =>
                slices.get(resolvedMethod).flatMap { calleeSlices =>
                  calleeSlices.find { s =>
                    s.targetObj match {
                      case p: ParamDef => p.name == argName
                      case _           => false
                    }
                  }
                }
              case ObservedCallWithArgPos(_, Some(resolvedMethod), _, _, Right(argIdx), _, _) =>
                slices.get(resolvedMethod).flatMap { calleeSlices =>
                  calleeSlices.find { s =>
                    s.targetObj match {
                      case p: ParamDef => p.position == argIdx
                      case _           => false
                    }
                  }
                }
              case _ => None
            }
            .foreach { s =>
              // todo: Handle slice linking
            }
        }
      }
    }
  }

  /** A field of `this` that gets assigned in a method, e.g. a class field with an initializer which ends up in the
    * constructor as `this.loader = new Loader()`.
    *
    * @param member
    *   the field as member of the type declaration.
    * @param method
    *   the method that assigns it. This is the scope of the slice.
    * @param assignment
    *   the (first) assignment to the field in the method.
    * @param access
    *   the left-hand side of the assignment.
    */
  private case class FieldDefinition(member: Member, method: Method, assignment: Call, access: Call) {
    def name: String = member.name
  }

  private def isThisAccessOf(name: String, node: AstNode): Boolean = node match {
    case c: Call if c.name == Operators.fieldAccess =>
      c.argument.l match {
        case List(base: Identifier, field: FieldIdentifier) => base.name == "this" && field.canonicalName == name
        case _                                              => false
      }
    case _ => false
  }

  private def isAccessOf(field: FieldDefinition, node: AstNode): Boolean = isThisAccessOf(field.name, node)

  private def fieldDefinitions(method: Method): List[FieldDefinition] = {
    val members = method.definingTypeDecl.member.l
    method.ast.isCall
      .nameExact(Operators.assignment)
      .flatMap { assignment =>
        assignment.argument
          .argumentIndex(1)
          .headOption
          .collect { case access: Call =>
            members
              .find(member => isThisAccessOf(member.name, access))
              .map(FieldDefinition(_, method, assignment, access))
          }
          .flatten
      }
      .l
      .distinctBy(_.name)
  }

  /** The calls (operators included) a field takes part in within the method assigning it.
    */
  private def getInCallsForField(field: FieldDefinition): List[Call] =
    field.method.ast.isCall
      .filter(isAccessOf(field, _))
      .inCall
      .flatMap {
        case call if call.name.startsWith(Operators.assignment) && call.ast.isCall.name(Operators.alloc).nonEmpty =>
          Some(call)
        case call if call.isFieldAccess                            => None
        case call if excludeOperatorCalls.get() && call.isOperator => None
        case call                                                  => Some(call)
      }
      .dedup
      .toList

  private def getInCallsForReferencedIdentifiers(decl: Declaration): List[Call] = {
    // Cross closure boundaries
    val capturedVars = decl.capturedByMethodRef.referencedMethod.ast.isIdentifier.nameExact(decl.name)
    decl
      .flatMap {
        case local: Local             => local.referencingIdentifiers ++ capturedVars
        case param: MethodParameterIn => param.referencingIdentifiers ++ capturedVars
        case _                        => Seq()
      }
      .inCall
      .flatMap {
        case c if c.name.startsWith(Operators.assignment) && c.ast.isCall.name(Operators.alloc).nonEmpty => Some(c)
        case c if c.isFieldAccess                                                                        => None
        case c if excludeOperatorCalls.get() && c.isOperator                                             => None
        case c                                                                                           => Some(c)
      }
      .dedup
      .toList
  }

  /** Returns true if the given declaration is found to have at least n non-operator calls within its referenced
    * identifiers' scope.
    *
    * @param decl
    *   the declaration to check.
    * @param n
    *   number of calls.
    * @return
    *   true if the call count condition is satisfied.
    */
  private def atLeastNCalls(decl: Declaration, n: Int): Boolean =
    getInCallsForReferencedIdentifiers(decl).size >= n

  /** Discovers internally defined types.
    *
    * @param cpg
    *   the CPG to query for types.
    * @return
    *   a list of user defined types.
    */
  def userDefinedTypes(cpg: Cpg): List[UserDefinedType] = {

    def generateUDT(typeDecl: TypeDecl): UserDefinedType = {
      UserDefinedType(
        typeDecl.fullName,
        typeDecl.member.map(m => DefComponent.fromNode(m)).collectAll[LocalDef].l,
        typeDecl.method
          .map(m =>
            ObservedCall(
              m.name,
              Option(m.fullName),
              m.parameter.map(_.typeFullName).toList,
              m.methodReturn.typeFullName,
              m.lineNumber.map(_.intValue()),
              m.columnNumber.map(_.intValue())
            )
          )
          .l,
        typeDecl.filename,
        typeDecl.lineNumber.map(_.intValue()),
        typeDecl.columnNumber.map(_.intValue())
      )
    }

    cpg.typeDecl
      .filterNot(t => t.isExternal || t.name.matches("(:program|<module>|<init>|<meta>|<body>)"))
      .map(generateUDT)
      .filter(udt => udt.fields.nonEmpty || udt.procedures.nonEmpty)
      .l
  }

  /** Adds extensions to extract all declaration nodes from a method body.
    */
  implicit class MethodDataSourceExt(trav: Iterator[Method]) {
    def declaration: Iterator[Declaration] = trav.ast.collectAll[Declaration]

  }

  implicit class CallExt(node: Call) {
    def isOperator: Boolean = node.name.startsWith("<operator")

    def isFieldAccess: Boolean = node.name == Operators.fieldAccess

    def isConstructor: Boolean = node.ast.isCall.name("<operator>\\.(new|alloc)").nonEmpty

  }

}
