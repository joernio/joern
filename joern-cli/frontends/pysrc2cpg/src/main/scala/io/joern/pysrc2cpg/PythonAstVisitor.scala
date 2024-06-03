package io.joern.pysrc2cpg

import PythonAstVisitor.{logger, metaClassSuffix, noLineAndColumn}
import io.joern.pysrc2cpg.memop.*
import io.joern.pysrc2cpg.Constants.builtinPrefix
import io.joern.pythonparser.ast
import io.joern.x2cpg.{AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewIdentifier, NewNode, NewTypeDecl}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable

object MethodParameters {
  def empty(): MethodParameters = {
    new MethodParameters(0, Nil)
  }
}
case class MethodParameters(posStartIndex: Int, positionalParams: Iterable[nodes.NewMethodParameterIn])

sealed trait PythonVersion
object PythonV2      extends PythonVersion
object PythonV3      extends PythonVersion
object PythonV2AndV3 extends PythonVersion

class PythonAstVisitor(
  relFileName: String,
  protected val nodeToCode: NodeToCode,
  version: PythonVersion,
  enableFileContent: Boolean
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(relFileName)
    with PythonAstVisitorHelpers {

  private val diffGraph     = new DiffGraphBuilder()
  protected val nodeBuilder = new NodeBuilder(diffGraph)
  protected val edgeBuilder = new EdgeBuilder(diffGraph)

  protected val contextStack = new ContextStack()

  private var memOpMap: AstNodeToMemoryOperationMap = scala.compiletime.uninitialized

  private val members = mutable.Map.empty[NewTypeDecl, List[String]]

  // As key only ast.FunctionDef and ast.AsyncFunctionDef are used but there
  // is no more specific type than ast.istmt.
  private val functionDefToMethod = mutable.Map.empty[ast.istmt, nodes.NewMethod]

  override def createAst(): DiffGraphBuilder = diffGraph

  private def createIdentifierLinks(): Unit = {
    contextStack.createIdentifierLinks(
      nodeBuilder.localNode,
      nodeBuilder.closureBindingNode,
      edgeBuilder.astEdge,
      edgeBuilder.refEdge,
      edgeBuilder.captureEdge
    )
  }

  def convert(astNode: ast.iast): NewNode = {
    astNode match {
      case module: ast.Module => convert(module)
    }
  }

  def convert(mod: ast.imod): NewNode = {
    mod match {
      case node: ast.Module => convert(node)
    }
  }

  // Entry method for the visitor.
  def convert(module: ast.Module): NewNode = {
    val memOpCalculator = new MemoryOperationCalculator()
    module.accept(memOpCalculator)
    memOpMap = memOpCalculator.astNodeToMemOp

    val contentOption = if (enableFileContent) {
      Some(nodeToCode.content)
    } else {
      None
    }
    val fileNode = nodeBuilder.fileNode(relFileName, contentOption)
    val namespaceBlockNode =
      nodeBuilder.namespaceBlockNode(
        Constants.GLOBAL_NAMESPACE,
        relFileName + ":" + Constants.GLOBAL_NAMESPACE,
        relFileName
      )
    edgeBuilder.astEdge(namespaceBlockNode, fileNode, 1)
    contextStack.setFileNamespaceBlock(namespaceBlockNode)

    val methodFullName = calculateFullNameFromContext("<module>")

    val firstLineAndCol = module.stmts.headOption.map(lineAndColOf)
    val lastLineAndCol  = module.stmts.lastOption.map(lineAndColOf)
    val line            = firstLineAndCol.map(_.line).getOrElse(1)
    val column          = firstLineAndCol.map(_.column).getOrElse(1)
    val offset          = firstLineAndCol.map(_.offset).getOrElse(1)
    val endLine         = lastLineAndCol.map(_.endLine).getOrElse(1)
    val endColumn       = lastLineAndCol.map(_.endColumn).getOrElse(1)
    val endOffset       = lastLineAndCol.map(_.endOffset).getOrElse(1)

    val moduleMethodNode =
      createMethod(
        "<module>",
        methodFullName,
        Some("<module>"),
        ModifierTypes.VIRTUAL :: ModifierTypes.MODULE :: Nil,
        parameterProvider = () => MethodParameters.empty(),
        bodyProvider = () => createBuiltinIdentifiers(memOpCalculator.names) ++ module.stmts.map(convert),
        returns = None,
        isAsync = false,
        methodRefNode = None,
        returnTypeHint = None,
        LineAndColumn(line, column, endLine, endColumn, offset, endOffset)
      )

    createIdentifierLinks()

    moduleMethodNode
  }

  // Create assignments of type references to all builtin identifiers if the identifier appears
  // at least once in the module. We filter in order to not generate the complete list of
  // assignment in each module.
  // That logic still generates assignments in cases where we would not need to, but figuring
  // that out would mean we need to wait until all identifiers are linked which is than too
  // late to create new identifiers and still use the same link mechanism. We would need
  // to rearrange quite some code to accomplish that. So we leave that as an optional TODO.
  // Note that namesUsedInModule is only calculated from ast.Name nodes! So e.g. new names
  // artificially created during lowering are not in that collection which is fine for now.
  private def createBuiltinIdentifiers(namesUsedInModule: collection.Set[String]): Iterable[nodes.NewNode] = {
    val result        = mutable.ArrayBuffer.empty[nodes.NewNode]
    val lineAndColumn = LineAndColumn(1, 1, 1, 1, 1, 1)

    val builtinFunctions = mutable.ArrayBuffer.empty[String]
    val builtinClasses   = mutable.ArrayBuffer.empty[String]

    if (version == PythonV3 || version == PythonV2AndV3) {
      builtinFunctions.appendAll(PythonAstVisitor.builtinFunctionsV3)
      builtinClasses.appendAll(PythonAstVisitor.builtinClassesV3)
    }
    if (version == PythonV2 || version == PythonV2AndV3) {
      builtinFunctions.appendAll(PythonAstVisitor.builtinFunctionsV2)
      builtinClasses.appendAll(PythonAstVisitor.builtinClassesV2)
    }

    builtinFunctions.distinct.foreach { builtinObjectName =>
      if (namesUsedInModule.contains(builtinObjectName)) {
        val assignmentNode = createAssignment(
          createIdentifierNode(builtinObjectName, Store, lineAndColumn),
          nodeBuilder
            .typeRefNode("__builtins__." + builtinObjectName, builtinPrefix + builtinObjectName, lineAndColumn),
          lineAndColumn
        )

        result.append(assignmentNode)
      }
    }

    builtinClasses.distinct.foreach { builtinObjectName =>
      if (namesUsedInModule.contains(builtinObjectName)) {
        val assignmentNode = createAssignment(
          createIdentifierNode(builtinObjectName, Store, lineAndColumn),
          nodeBuilder.typeRefNode(
            "__builtins__." + builtinObjectName,
            builtinPrefix + builtinObjectName + metaClassSuffix,
            lineAndColumn
          ),
          lineAndColumn
        )

        result.append(assignmentNode)
      }
    }

    result
  }

  private def unhandled(node: ast.iast & ast.iattributes): NewNode = {
    val unhandledAsUnknown = true
    if (unhandledAsUnknown) {
      nodeBuilder.unknownNode(node.toString, node.getClass.getName, lineAndColOf(node))
    } else {
      throw new NotImplementedError()
    }
  }

  def convert(stmt: ast.istmt): NewNode = {
    stmt match {
      case node: ast.FunctionDef      => convert(node)
      case node: ast.AsyncFunctionDef => convert(node)
      case node: ast.ClassDef         => convert(node)
      case node: ast.Return           => convert(node)
      case node: ast.Delete           => convert(node)
      case node: ast.Assign           => convert(node)
      case node: ast.TypeAlias        => unhandled(node)
      case node: ast.AnnAssign        => convert(node)
      case node: ast.AugAssign        => convert(node)
      case node: ast.For              => convert(node)
      case node: ast.AsyncFor         => convert(node)
      case node: ast.While            => convert(node)
      case node: ast.If               => convert(node)
      case node: ast.With             => convert(node)
      case node: ast.AsyncWith        => convert(node)
      case node: ast.Match            => convert(node)
      case node: ast.Raise            => convert(node)
      case node: ast.Try              => convert(node)
      case node: ast.Assert           => convert(node)
      case node: ast.Import           => convert(node)
      case node: ast.ImportFrom       => convert(node)
      case node: ast.Global           => convert(node)
      case node: ast.Nonlocal         => convert(node)
      case node: ast.Expr             => convert(node)
      case node: ast.Pass             => convert(node)
      case node: ast.Break            => convert(node)
      case node: ast.Continue         => convert(node)
      case node: ast.RaiseP2          => unhandled(node)
      case node: ast.ErrorStatement   => convert(node)
    }
  }

  def convert(functionDef: ast.FunctionDef): NewNode = {
    val methodIdentifierNode =
      createIdentifierNode(functionDef.name, Store, lineAndColOf(functionDef))
    val (methodNode, methodRefNode) = createMethodAndMethodRef(
      functionDef.name,
      Some(functionDef.name),
      createParameterProcessingFunction(functionDef.args, isStaticMethod(functionDef.decorator_list)),
      () => functionDef.body.map(convert),
      functionDef.returns,
      isAsync = false,
      lineAndColOf(functionDef)
    )
    functionDefToMethod.put(functionDef, methodNode)

    val wrappedMethodRefNode =
      wrapMethodRefWithDecorators(methodRefNode, functionDef.decorator_list)

    createAssignment(methodIdentifierNode, wrappedMethodRefNode, lineAndColOf(functionDef))
  }

  /*
   * For a decorated function like:
   * @f1(arg)
   * @f2
   * def func(): pass
   *
   * The lowering is:
   * func = f1(arg)(f2(func))
   *
   * This function takes a method ref, wraps it in the decorator calls and returns the resulting expression.
   * In the example case this is:
   * f1(arg)(f2(func))
   */
  def wrapMethodRefWithDecorators(methodRefNode: nodes.NewNode, decoratorList: Iterable[ast.iexpr]): nodes.NewNode = {
    decoratorList.foldRight(methodRefNode)((decorator: ast.iexpr, wrappedMethodRef: nodes.NewNode) =>
      val (decoratorNode, decoratorName) = convert(decorator) match {
        case decoratorNode: NewIdentifier => decoratorNode -> decoratorNode.name
        case decoratorNode                => decoratorNode -> "" // other decorators are dynamic so we leave this blank
      }
      createCall(decoratorNode, decoratorName, lineAndColOf(decorator), wrappedMethodRef :: Nil, Nil)
    )
  }

  def convert(functionDef: ast.AsyncFunctionDef): NewNode = {
    val methodIdentifierNode =
      createIdentifierNode(functionDef.name, Store, lineAndColOf(functionDef))
    val (methodNode, methodRefNode) = createMethodAndMethodRef(
      functionDef.name,
      Some(functionDef.name),
      createParameterProcessingFunction(functionDef.args, isStaticMethod(functionDef.decorator_list)),
      () => functionDef.body.map(convert),
      functionDef.returns,
      isAsync = true,
      lineAndColOf(functionDef)
    )
    functionDefToMethod.put(functionDef, methodNode)

    val wrappedMethodRefNode =
      wrapMethodRefWithDecorators(methodRefNode, functionDef.decorator_list)

    createAssignment(methodIdentifierNode, wrappedMethodRefNode, lineAndColOf(functionDef))
  }

  private def isStaticMethod(decoratorList: Iterable[ast.iexpr]): Boolean = {
    decoratorList.exists {
      case name: ast.Name if name.id == "staticmethod" => true
      case _                                           => false
    }
  }

  private def isClassMethod(decoratorList: Iterable[ast.iexpr]): Boolean = {
    decoratorList.exists {
      case name: ast.Name if name.id == "classmethod" => true
      case _                                          => false
    }
  }

  private def createParameterProcessingFunction(
    parameters: ast.Arguments,
    isStatic: Boolean
  ): () => MethodParameters = {
    val startIndex =
      if (contextStack.isClassContext && !isStatic)
        0
      else
        1

    () => new MethodParameters(startIndex, convert(parameters, startIndex))
  }

  // TODO handle returns
  private def createMethodAndMethodRef(
    methodName: String,
    scopeName: Option[String],
    parameterProvider: () => MethodParameters,
    bodyProvider: () => Iterable[nodes.NewNode],
    returns: Option[ast.iexpr],
    isAsync: Boolean,
    lineAndColumn: LineAndColumn,
    additionalModifiers: List[String] = List.empty
  ): (nodes.NewMethod, nodes.NewMethodRef) = {
    val methodFullName = calculateFullNameFromContext(methodName)

    val methodRefNode =
      nodeBuilder.methodRefNode("def " + methodName + "(...)", methodFullName, lineAndColumn)

    val methodNode =
      createMethod(
        methodName,
        methodFullName,
        scopeName,
        ModifierTypes.VIRTUAL :: additionalModifiers,
        parameterProvider,
        bodyProvider,
        returns,
        isAsync = true,
        Some(methodRefNode),
        returnTypeHint = None,
        lineAndColumn
      )

    (methodNode, methodRefNode)
  }

  // It is important that the nodes returned by all provider function are created
  // during the function invocation and not in advance. Because only
  // than the context information is correct.
  private def createMethod(
    name: String,
    fullName: String,
    scopeName: Option[String],
    modifiers: List[String],
    parameterProvider: () => MethodParameters,
    bodyProvider: () => Iterable[nodes.NewNode],
    returns: Option[ast.iexpr],
    isAsync: Boolean,
    methodRefNode: Option[nodes.NewMethodRef],
    returnTypeHint: Option[String],
    lineAndColumn: LineAndColumn
  ): nodes.NewMethod = {
    val methodNode = nodeBuilder.methodNode(name, fullName, relFileName, lineAndColumn)
    edgeBuilder.astEdge(methodNode, contextStack.astParent, contextStack.order.getAndInc)

    val blockNode = nodeBuilder.blockNode("", lineAndColumn)
    edgeBuilder.astEdge(blockNode, methodNode, 1)

    contextStack.pushMethod(scopeName, methodNode, blockNode, methodRefNode)

    var order = 0
    for (modifier <- modifiers) {
      val modifierNode = nodeBuilder.modifierNode(modifier)
      edgeBuilder.astEdge(modifierNode, methodNode, order)
      order += 1
    }

    val methodParameter = parameterProvider()
    val parameterOrder  = new AutoIncIndex(methodParameter.posStartIndex)

    methodParameter.positionalParams.foreach { parameterNode =>
      contextStack.addParameter(parameterNode)
      edgeBuilder.astEdge(parameterNode, methodNode, parameterOrder.getAndInc)
    }

    val methodReturnNode =
      nodeBuilder.methodReturnNode(nodeBuilder.extractTypesFromHint(returns), returnTypeHint, lineAndColumn)
    edgeBuilder.astEdge(methodReturnNode, methodNode, 2)

    val bodyOrder = new AutoIncIndex(1)
    bodyProvider().foreach { bodyStmt =>
      edgeBuilder.astEdge(bodyStmt, blockNode, bodyOrder.getAndInc)
    }

    // For every method we create a corresponding TYPE and TYPE_DECL and
    // a binding for the method into TYPE_DECL.
    val typeNode     = nodeBuilder.typeNode(name, fullName)
    val typeDeclNode = nodeBuilder.typeDeclNode(name, fullName, relFileName, Seq(Constants.ANY), lineAndColumn)

    // For every method that is a module, the local variables can be imported by other modules. This behaviour is
    // much like fields so they are to be linked as fields to this method type
    if (name == "<module>") contextStack.createMemberLinks(typeDeclNode, edgeBuilder.astEdge)

    contextStack.pop()
    edgeBuilder.astEdge(typeDeclNode, contextStack.astParent, contextStack.order.getAndInc)
    createBinding(methodNode, typeDeclNode)

    methodNode
  }

  // For a classDef we do:
  // 1. Create a metaType, metaTypeDecl and metaTypeRef.
  // 2. Create a function containing the code of the classDef body.
  // 3. Create a block which contains a call to the body function
  //    and an assignment of the metaTypeRef to an identifier with the class name.
  // 4. Create type and typeDecl for the instance class.
  // 5. Create and link members in metaTypeDecl and instanceTypeDecl
  def convert(classDef: ast.ClassDef): NewNode = {
    // Create type for the meta class object
    val metaTypeDeclName     = classDef.name + metaClassSuffix
    val metaTypeDeclFullName = calculateFullNameFromContext(metaTypeDeclName)

    val metaTypeNode = nodeBuilder.typeNode(metaTypeDeclName, metaTypeDeclFullName)
    val metaTypeDeclNode =
      nodeBuilder.typeDeclNode(
        metaTypeDeclName,
        metaTypeDeclFullName,
        relFileName,
        Seq(Constants.ANY),
        lineAndColOf(classDef)
      )
    edgeBuilder.astEdge(metaTypeDeclNode, contextStack.astParent, contextStack.order.getAndInc)

    // Create type for class instances
    val instanceTypeDeclName     = classDef.name
    val instanceTypeDeclFullName = calculateFullNameFromContext(instanceTypeDeclName)

    // TODO for now we just take the code of the base expression and pretend they are full names, converting special
    //  nodes as we go.
    def handleInheritance(fs: List[ast.iexpr]): List[String] = fs match {
      case (x: ast.Call) :: xs =>
        val node       = convert(x)
        val parent     = contextStack.astParent
        val tmpVar     = createIdentifierNode(getUnusedName(), Store, lineAndColOf(x))
        val assignment = createAssignment(tmpVar, node, lineAndColOf(x))
        diffGraph.addEdge(parent, assignment, EdgeTypes.AST)
        tmpVar.name +: handleInheritance(xs)
      case x :: xs =>
        nodeToCode.getCode(x) +: handleInheritance(xs)
      case Nil => Nil
    }

    val inheritsFrom = handleInheritance(classDef.bases.toList)

    val instanceType = nodeBuilder.typeNode(instanceTypeDeclName, instanceTypeDeclFullName)
    val instanceTypeDecl =
      nodeBuilder.typeDeclNode(
        instanceTypeDeclName,
        instanceTypeDeclFullName,
        relFileName,
        inheritsFrom,
        lineAndColOf(classDef)
      )
    edgeBuilder.astEdge(instanceTypeDecl, contextStack.astParent, contextStack.order.getAndInc)

    // Create <body> function which contains the code defining the class
    contextStack.pushClass(Some(classDef.name), instanceTypeDecl)
    val classBodyFunctionName = "<body>"
    val (_, methodRefNode) = createMethodAndMethodRef(
      classBodyFunctionName,
      scopeName = None,
      parameterProvider = () => MethodParameters.empty(),
      bodyProvider = () => classDef.body.map(convert),
      None,
      isAsync = false,
      lineAndColOf(classDef)
    )

    contextStack.pop()

    contextStack.pushClass(Some(classDef.name), metaTypeDeclNode)

    // Create meta class call handling method and bind it to meta class type.
    val functions = classDef.body.collect { case func: ast.FunctionDef => func }

    // __init__ method has to be in functions because "async def __init__" is invalid.
    val initFunctionOption = functions.find(_.name == "__init__")

    val initParameters = initFunctionOption.map(_.args).getOrElse {
      // Create arguments of a default __init__ function.
      ast.Arguments(
        posonlyargs = mutable.Seq.empty[ast.Arg],
        args = mutable.Seq(ast.Arg("self", None, None, classDef.attributeProvider)),
        vararg = None,
        kwonlyargs = mutable.Seq.empty[ast.Arg],
        kw_defaults = mutable.Seq.empty[Option[ast.iexpr]],
        kw_arg = None,
        defaults = mutable.Seq.empty[ast.iexpr]
      )
    }

    val metaClassCallHandlerMethod =
      createMetaClassCallHandlerMethod(initParameters, metaTypeDeclName, metaTypeDeclFullName, instanceTypeDeclFullName)

    createBinding(metaClassCallHandlerMethod, metaTypeDeclNode)

    // Create fake __new__ regardless whether there is an actual implementation in the code.
    // We do this to model the __init__ call in a visible way for the data flow tracker.
    // This is done because very often the __init__ call is hidden in a super().__new__ call
    // and we cant yet handle super().
    val fakeNewMethod = createFakeNewMethod(initParameters)

    val fakeNewMember = nodeBuilder.memberNode("<fakeNew>", fakeNewMethod.fullName)
    edgeBuilder.astEdge(fakeNewMember, metaTypeDeclNode, contextStack.order.getAndInc)

    // Create binding into class instance type for each method.
    // Also create bindings into meta class type to enable calls like "MyClass.func(obj, p1)".
    // For non static methods we create an adapter method which basically only shifts the parameters
    // one to the left and makes sure that the meta class object is not passed to func as instance
    // parameter.
    classDef.body.foreach {
      case func: ast.FunctionDef =>
        createMemberBindingsAndAdapter(
          func,
          func.name,
          func.args,
          func.decorator_list,
          instanceTypeDecl,
          metaTypeDeclNode
        )
      case func: ast.AsyncFunctionDef =>
        createMemberBindingsAndAdapter(
          func,
          func.name,
          func.args,
          func.decorator_list,
          instanceTypeDecl,
          metaTypeDeclNode
        )
      case _ =>
      // All other body statements are currently ignored.
    }

    contextStack.pop()

    // Create call to <body> function and assignment of the meta class object to a identifier named
    // like the class.
    val callToClassBodyFunction = createCall(methodRefNode, "", lineAndColOf(classDef), Nil, Nil)
    val metaTypeRefNode =
      createTypeRef(metaTypeDeclName, metaTypeDeclFullName, lineAndColOf(classDef))
    val classIdentifierAssignNode =
      createAssignmentToIdentifier(classDef.name, metaTypeRefNode, lineAndColOf(classDef))

    val classBlock = createBlock(callToClassBodyFunction :: classIdentifierAssignNode :: Nil, lineAndColOf(classDef))

    classBlock
  }

  private def createMemberBindingsAndAdapter(
    function: ast.istmt,
    functionName: String,
    functionArgs: ast.Arguments,
    functionDecoratorList: Iterable[ast.iexpr],
    instanceTypeDecl: nodes.NewNode,
    metaTypeDecl: nodes.NewNode
  ): Unit = {
    val memberForInstance =
      nodeBuilder.memberNode(functionName, functionDefToMethod.apply(function).fullName, lineAndColOf(function))
    edgeBuilder.astEdge(memberForInstance, instanceTypeDecl, contextStack.order.getAndInc)

    val methodForMetaClass =
      if (isStaticMethod(functionDecoratorList) || isClassMethod(functionDecoratorList)) {
        functionDefToMethod.apply(function)
      } else {
        createMetaClassAdapterMethod(
          functionName,
          functionDefToMethod.apply(function).fullName,
          functionArgs,
          lineAndColOf(function)
        )
      }

    val memberForMeta = nodeBuilder.memberNode(functionName, methodForMetaClass.fullName, lineAndColOf(function))
    edgeBuilder.astEdge(memberForMeta, metaTypeDecl, contextStack.order.getAndInc)
  }

  /** Creates an adapter method which adapts the meta class version of a method to the instance class version. Consider
    * class: class MyClass(): def func(self, p1): pass
    *
    * The syntax to call func via the meta class is: MyClass.func(someInstance, p1), whereas the call via the instance
    * itself is: someInstance.func(p1). To adapt between those two we generate: def func<metaClassAdapter>(cls, self,
    * p1): return STATIC_CALL(MyClass.func(self, p1))
    * @return
    */
  // TODO handle kwArg
  private def createMetaClassAdapterMethod(
    adaptedMethodName: String,
    adaptedMethodFullName: String,
    parameters: ast.Arguments,
    lineAndColumn: LineAndColumn
  ): nodes.NewMethod = {
    val adapterMethodName     = adaptedMethodName + "<metaClassAdapter>"
    val adapterMethodFullName = calculateFullNameFromContext(adapterMethodName)

    createMethod(
      adapterMethodName,
      adapterMethodFullName,
      Some(adaptedMethodName),
      ModifierTypes.VIRTUAL :: Nil,
      parameterProvider = () => {
        MethodParameters(
          0,
          nodeBuilder.methodParameterNode("cls", isVariadic = false, lineAndColumn, Option(0)) :: Nil ++
            convert(parameters, 1)
        )
      },
      bodyProvider = () => {
        val (arguments, keywordArguments) = createArguments(parameters, lineAndColumn)
        val staticCall =
          createStaticCall(adaptedMethodName, adaptedMethodFullName, lineAndColumn, arguments, keywordArguments)
        val returnNode = createReturn(Some(staticCall), None, lineAndColumn)
        returnNode :: Nil
      },
      returns = None,
      isAsync = false,
      methodRefNode = None,
      returnTypeHint = None,
      lineAndColumn
    )
  }

  def createArguments(
    arguments: ast.Arguments,
    lineAndColumn: LineAndColumn
  ): (Iterable[nodes.NewNode], Iterable[(String, nodes.NewNode)]) = {
    val convertedArgs        = mutable.ArrayBuffer.empty[nodes.NewNode]
    val convertedKeywordArgs = mutable.ArrayBuffer.empty[(String, nodes.NewNode)]

    arguments.posonlyargs.foreach { arg =>
      convertedArgs.append(createIdentifierNode(arg.arg, Load, lineAndColumn))
    }
    arguments.args.foreach { arg =>
      convertedArgs.append(createIdentifierNode(arg.arg, Load, lineAndColumn))
    }
    arguments.vararg.foreach { arg =>
      convertedArgs.append(
        createStarredUnpackOperatorCall(createIdentifierNode(arg.arg, Load, lineAndColumn), lineAndColumn)
      )
    }
    arguments.kwonlyargs.foreach { arg =>
      convertedKeywordArgs.append((arg.arg, createIdentifierNode(arg.arg, Load, lineAndColumn)))
    }

    (convertedArgs, convertedKeywordArgs)
  }

  /** This function strips the first positional parameter from initParameters, if present.
    * @return
    *   Parameters without first positional parameter and adjusted line and column number information.
    */
  private def stripFirstPositionalParameter(initParameters: ast.Arguments): (ast.Arguments, LineAndColumn) = {
    if (initParameters.posonlyargs.nonEmpty) {
      (
        initParameters.copy(posonlyargs = initParameters.posonlyargs.tail),
        lineAndColOf(initParameters.posonlyargs.head)
      )
    } else if (initParameters.args.nonEmpty) {
      (initParameters.copy(args = initParameters.args.tail), lineAndColOf(initParameters.args.head))
    } else if (initParameters.vararg.nonEmpty) {
      (initParameters, lineAndColOf(initParameters.vararg.get))
    } else {
      (initParameters, lineAndColOf(initParameters.kw_arg.get))
    }
  }

  /** Creates the method which handles a call to the meta class object. This process is also known as creating a new
    * instance object, e.g. obj = MyClass(p1). The purpose of the generated function is to adapt between the special
    * cased instance creation call and a normal call to __new__ (for now <fakeNew>). The adaption is required to in
    * order to provide TYPE_REF(meta class) as instance argument to __new__/<fakeNew>. So the <metaClassCallHandler>
    * looks like: def <metaClassCallHandler>(p1): return DYNAMIC_CALL(receiver=TYPE_REF(meta class).<fakeNew>, instance
    * \= TYPE_REF(meta class), p1)
    */
  // TODO handle kwArg
  private def createMetaClassCallHandlerMethod(
    initParameters: ast.Arguments,
    metaTypeDeclName: String,
    metaTypeDeclFullName: String,
    instanceTypeDeclFullName: String
  ): nodes.NewMethod = {
    val methodName     = "<metaClassCallHandler>"
    val methodFullName = calculateFullNameFromContext(methodName)

    // We need to drop the "self" parameter either from the position only or normal parameters
    // because "self" is not passed through but rather created in __new__.
    val (parametersWithoutSelf, lineAndColumn) = stripFirstPositionalParameter(initParameters)

    createMethod(
      methodName,
      methodFullName,
      Some(methodName),
      ModifierTypes.VIRTUAL :: Nil,
      parameterProvider = () => {
        MethodParameters(1, convert(parametersWithoutSelf, 1))
      },
      bodyProvider = () => {
        val (arguments, keywordArguments) = createArguments(parametersWithoutSelf, lineAndColumn)

        val fakeNewCall = createInstanceCall(
          createFieldAccess(
            createTypeRef(metaTypeDeclName, metaTypeDeclFullName, lineAndColumn),
            "<fakeNew>",
            lineAndColumn
          ),
          createTypeRef(metaTypeDeclName, metaTypeDeclFullName, lineAndColumn),
          "",
          lineAndColumn,
          arguments,
          keywordArguments
        )

        val returnNode = createReturn(Some(fakeNewCall), None, lineAndColumn)

        returnNode :: Nil
      },
      returns = None,
      isAsync = false,
      methodRefNode = None,
      returnTypeHint = Some(instanceTypeDeclFullName),
      lineAndColumn
    )
  }

  /** Creates a <fakeNew> method which mimics the behaviour of a default __new__ method (the one you would get if no
    * implementation is present). The reason we use a fake version of the __new__ method it that we wont be able to
    * correctly track through most custom __new__ implementations as they usually call "super.__init__()" and we cannot
    * yet handle "super". The fake __new__ looks like: def <fakeNew>(cls, p1): __newInstance =
    * STATIC_CALL(<operator>.alloc) cls.__init__(__newIstance, p1) return __newInstance
    */
  // TODO handle kwArg
  private def createFakeNewMethod(initParameters: ast.Arguments): nodes.NewMethod = {
    val newMethodName         = "<fakeNew>"
    val newMethodStubFullName = calculateFullNameFromContext(newMethodName)

    // We need to drop the "self" parameter either from the position only or normal parameters
    // because "self" is not passed through but rather created in __new__.
    val (parametersWithoutSelf, lineAndColumn) = stripFirstPositionalParameter(initParameters)

    createMethod(
      newMethodName,
      newMethodStubFullName,
      Some(newMethodName),
      ModifierTypes.VIRTUAL :: Nil,
      parameterProvider = () => {
        MethodParameters(
          0,
          nodeBuilder.methodParameterNode("cls", isVariadic = false, lineAndColumn, Some(0)) :: Nil ++
            convert(parametersWithoutSelf, 1)
        )
      },
      bodyProvider = () => {
        val allocatorCall =
          createNAryOperatorCall(() => ("<operator>.alloc", "<operator>.alloc"), Nil, lineAndColumn)
        val assignmentToNewInstance =
          createAssignment(createIdentifierNode("__newInstance", Store, lineAndColumn), allocatorCall, lineAndColumn)

        val (arguments, keywordArguments) = createArguments(parametersWithoutSelf, lineAndColumn)
        val argumentWithInstance          = mutable.ArrayBuffer.empty[nodes.NewNode]
        argumentWithInstance.append(createIdentifierNode("__newInstance", Load, lineAndColumn))
        argumentWithInstance.appendAll(arguments)

        val initCall = createXDotYCall(
          () => createIdentifierNode("cls", Load, lineAndColumn),
          "__init__",
          xMayHaveSideEffects = false,
          lineAndColumn,
          argumentWithInstance,
          keywordArguments
        )

        val returnNode =
          createReturn(Some(createIdentifierNode("__newInstance", Load, lineAndColumn)), None, lineAndColumn)

        assignmentToNewInstance :: initCall :: returnNode :: Nil
      },
      returns = None,
      isAsync = false,
      methodRefNode = None,
      returnTypeHint = None,
      lineAndColumn
    )
  }

  def convert(ret: ast.Return): NewNode = {
    createReturn(ret.value.map(convert), Some(nodeToCode.getCode(ret)), lineAndColOf(ret))
  }

  def convert(delete: ast.Delete): NewNode = {
    val deleteArgs = delete.targets.map(convert)

    val code     = "del " + deleteArgs.map(codeOf).mkString(", ")
    val callNode = nodeBuilder.callNode(code, "<operator>.delete", DispatchTypes.STATIC_DISPATCH, lineAndColOf(delete))

    addAstChildrenAsArguments(callNode, 1, deleteArgs)
    callNode
  }

  def convert(assign: ast.Assign): nodes.NewNode = {
    val loweredNodes =
      createValueToTargetsDecomposition(assign.targets, convert(assign.value), lineAndColOf(assign))

    if (loweredNodes.size == 1) {
      // Simple assignment can be returned directly.
      loweredNodes.head
    } else {
      createBlock(loweredNodes, lineAndColOf(assign))
    }
  }

  // TODO for now we ignore the annotation part and just emit the pure
  // assignment.
  def convert(annotatedAssign: ast.AnnAssign): NewNode = {
    val targetNode = convert(annotatedAssign.target)

    annotatedAssign.value match {
      case Some(value) =>
        val valueNode = convert(value)
        createAssignment(targetNode, valueNode, lineAndColOf(annotatedAssign))
      case None =>
        // If there is no value, this is just an expr: annotation and since
        // we for now ignore the annotation we emit just the expr because
        // it may have side effects.
        targetNode
    }
  }

  def convert(augAssign: ast.AugAssign): NewNode = {
    val targetNode = convert(augAssign.target)
    val valueNode  = convert(augAssign.value)

    val (operatorCode, operatorFullName) =
      augAssign.op match {
        case ast.Add  => ("+=", Operators.assignmentPlus)
        case ast.Sub  => ("-=", Operators.assignmentMinus)
        case ast.Mult => ("*=", Operators.assignmentMultiplication)
        case ast.MatMult =>
          ("@=", "<operator>.assignmentMatMult") // TODO make this a define and add policy for this
        case ast.Div    => ("/=", Operators.assignmentDivision)
        case ast.Mod    => ("%=", Operators.assignmentModulo)
        case ast.Pow    => ("**=", Operators.assignmentExponentiation)
        case ast.LShift => ("<<=", Operators.assignmentShiftLeft)
        case ast.RShift => ("<<=", Operators.assignmentArithmeticShiftRight)
        case ast.BitOr  => ("|=", Operators.assignmentOr)
        case ast.BitXor => ("^=", Operators.assignmentXor)
        case ast.BitAnd => ("&=", Operators.assignmentAnd)
        case ast.FloorDiv =>
          ("//=", "<operator>.assignmentFloorDiv") // TODO make this a define and add policy for this
      }

    createAugAssignment(targetNode, operatorCode, valueNode, operatorFullName, lineAndColOf(augAssign))
  }

  // TODO write test
  def convert(forStmt: ast.For): NewNode = {
    createForLowering(
      forStmt.target,
      forStmt.iter,
      Iterable.empty,
      forStmt.body.map(convert),
      forStmt.orelse.map(convert),
      isAsync = false,
      lineAndColOf(forStmt)
    )
  }

  def convert(forStmt: ast.AsyncFor): NewNode = {
    createForLowering(
      forStmt.target,
      forStmt.iter,
      Iterable.empty,
      forStmt.body.map(convert),
      forStmt.orelse.map(convert),
      isAsync = true,
      lineAndColOf(forStmt)
    )
  }

  // Lowering of for x in y: <statements>:
  // {
  //   iterator = y.__iter__()
  //   while (UNKNOWN condition):
  //     <loweringOf>(x = iterator.__next__())
  //     <statements>
  // }
  // If one "if" is present the lowering of for x in y if z:
  // {
  //   iterator = y.__iter__()
  //   while (UNKNOWN condition):
  //     if (!z): continue
  //     <loweringOf>(x = iterator.__next__())
  //     <statements>
  // }
  // If multiple "ifs" are present the lowering of for x in y if z if a: ..,:
  // {
  //   iterator = y.__iter__()
  //   while (UNKNOWN condition):
  //     if (!(z and a)): continue
  //     <loweringOf>(x = iterator.__next__())
  //     <statements>
  // }
  protected def createForLowering(
    target: ast.iexpr,
    iter: ast.iexpr,
    ifs: Iterable[ast.iexpr],
    bodyNodes: Iterable[nodes.NewNode],
    orelseNodes: Iterable[nodes.NewNode],
    isAsync: Boolean,
    lineAndColumn: LineAndColumn
  ): nodes.NewNode = {
    val iterVariableName = getUnusedName()
    val iterExprIterCallNode =
      createXDotYCall(
        () => convert(iter),
        "__iter__",
        xMayHaveSideEffects = !iter.isInstanceOf[ast.Name],
        lineAndColumn,
        Nil,
        Nil
      )
    val iterAssignNode =
      createAssignmentToIdentifier(iterVariableName, iterExprIterCallNode, lineAndColumn)

    val conditionNode = nodeBuilder.unknownNode("iteratorNonEmptyOrException", "", lineAndColumn)

    val controlStructureNode =
      nodeBuilder.controlStructureNode("while ... : ...", ControlStructureTypes.WHILE, lineAndColumn)
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)

    val iterNextCallNode =
      createXDotYCall(
        () => createIdentifierNode(iterVariableName, Load, lineAndColumn),
        "__next__",
        xMayHaveSideEffects = false,
        lineAndColumn,
        Nil,
        Nil
      )

    val loweredAssignNodes =
      createValueToTargetsDecomposition(Iterable.single(target), iterNextCallNode, lineAndColumn)

    val blockStmtNodes = mutable.ArrayBuffer.empty[nodes.NewNode]
    blockStmtNodes.appendAll(loweredAssignNodes)

    if (ifs.nonEmpty) {
      val conditionNode =
        if (ifs.size == 1) {
          ifs.head
        } else {
          ast.BoolOp(ast.And, ifs.to(mutable.Seq), ifs.head.attributeProvider)
        }
      val ifNotContinueNode = convert(
        ast.If(
          ast.UnaryOp(ast.Not, conditionNode, ifs.head.attributeProvider),
          mutable.ArrayBuffer.empty[ast.istmt].append(ast.Continue(ifs.head.attributeProvider)),
          mutable.Seq.empty[ast.istmt],
          ifs.head.attributeProvider
        )
      )

      blockStmtNodes.append(ifNotContinueNode)
    }
    bodyNodes.foreach(blockStmtNodes.append)

    val bodyBlockNode = createBlock(blockStmtNodes, lineAndColumn)
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode)

    if (orelseNodes.nonEmpty) {
      val elseBlockNode = createBlock(orelseNodes, lineAndColumn)
      addAstChildNodes(controlStructureNode, 3, elseBlockNode)
    }

    createBlock(iterAssignNode :: controlStructureNode :: Nil, lineAndColumn)
  }

  def convert(astWhile: ast.While): nodes.NewNode = {
    val conditionNode = convert(astWhile.test)
    val bodyStmtNodes = astWhile.body.map(convert)

    val controlStructureNode =
      nodeBuilder.controlStructureNode("while ... : ...", ControlStructureTypes.WHILE, lineAndColOf(astWhile))
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)

    val bodyBlockNode = createBlock(bodyStmtNodes, lineAndColOf(astWhile))
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode)

    if (astWhile.orelse.nonEmpty) {
      val elseStmtNodes = astWhile.orelse.map(convert)
      val elseBlockNode =
        createBlock(elseStmtNodes, lineAndColOf(astWhile.orelse.head))
      addAstChildNodes(controlStructureNode, 3, elseBlockNode)
    }

    controlStructureNode
  }

  def convert(astIf: ast.If): nodes.NewNode = {
    val conditionNode = convert(astIf.test)
    val bodyStmtNodes = astIf.body.map(convert)

    val controlStructureNode =
      nodeBuilder.controlStructureNode("if ... : ...", ControlStructureTypes.IF, lineAndColOf(astIf))
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)

    val bodyBlockNode = createBlock(bodyStmtNodes, lineAndColOf(astIf))
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode)

    if (astIf.orelse.nonEmpty) {
      val elseStmtNodes = astIf.orelse.map(convert)
      val elseBlockNode = createBlock(elseStmtNodes, lineAndColOf(astIf.orelse.head))
      addAstChildNodes(controlStructureNode, 3, elseBlockNode)
    }

    controlStructureNode
  }

  def convert(withStmt: ast.With): NewNode = {
    val loweredNodes =
      withStmt.items.foldRight(withStmt.body.map(convert)) { case (withItem, bodyStmts) =>
        mutable.ArrayBuffer.empty.append(convertWithItem(withItem, bodyStmts))
      }

    loweredNodes.head
  }

  def convert(withStmt: ast.AsyncWith): NewNode = {
    val loweredNodes =
      withStmt.items.foldRight(withStmt.body.map(convert)) { case (withItem, bodyStmts) =>
        mutable.ArrayBuffer.empty.append(convertWithItem(withItem, bodyStmts))
      }

    loweredNodes.head
  }

  // Handles the lowering of a single "with item". E.g. for: with A() as a, B() as b
  // "A() as a" is a single "with item".
  // The lowering for:
  //   with EXPRESSION as TARGET:
  //     SUITE
  // is:
  //   manager = (EXPRESSION)
  //   enter = manager.__enter__
  //   exit = manager.__exit__
  //   value = enter(manager)
  //
  //   try:
  //     TARGET = value
  //     SUITE
  //   finally:
  //     exit(manager)
  //
  // Note that this is not quite semantically correct because we ignore the exit method
  // arguments and the exit call return value. This is fine for us because for our data
  // flow tracking purposed we dont need that extra information and the AST is anyway
  // "broken" because we are heavily lowering.
  // For reference the following excerpt taken from the official Python 3.9.5
  // documentation found at
  // https://docs.python.org/3/reference/compound_stmts.html#the-with-statement
  // shows the semantically correct lowering:
  //   manager = (EXPRESSION)
  //   enter = type(manager).__enter__
  //   exit = type(manager).__exit__
  //   value = enter(manager)
  //   hit_except = False
  //
  //   try:
  //     TARGET = value
  //     SUITE
  //   except:
  //     hit_except = True
  //     if not exit(manager, *sys.exc_info()):
  //       raise
  //   finally:
  //     if not hit_except:
  //       exit(manager, None, None, None)
  private def convertWithItem(withItem: ast.Withitem, suite: collection.Seq[nodes.NewNode]): nodes.NewNode = {
    val lineAndCol            = lineAndColOf(withItem.context_expr)
    val managerIdentifierName = getUnusedName("manager")

    val assignmentToManager =
      createAssignmentToIdentifier(managerIdentifierName, convert(withItem.context_expr), lineAndCol)

    val enterIdentifierName = getUnusedName("enter")
    val assignmentToEnter = createAssignmentToIdentifier(
      enterIdentifierName,
      createFieldAccess(createIdentifierNode(managerIdentifierName, Load, lineAndCol), "__enter__", lineAndCol),
      lineAndCol
    )

    val exitIdentifierName = getUnusedName("exit")
    val assignmentToExit = createAssignmentToIdentifier(
      exitIdentifierName,
      createFieldAccess(createIdentifierNode(managerIdentifierName, Load, lineAndCol), "__exit__", lineAndCol),
      lineAndCol
    )

    val valueIdentifierName = getUnusedName("value")
    val assignmentToValue = createAssignmentToIdentifier(
      valueIdentifierName,
      createInstanceCall(
        createIdentifierNode(enterIdentifierName, Load, lineAndCol),
        createIdentifierNode(managerIdentifierName, Load, lineAndCol),
        "",
        lineAndCol,
        Nil,
        Nil
      ),
      lineAndCol
    )

    val tryBody =
      withItem.optional_vars match {
        case Some(optionalVar) =>
          val loweredTargetAssignNodes = createValueToTargetsDecomposition(
            withItem.optional_vars,
            createIdentifierNode(valueIdentifierName, Load, lineAndCol),
            lineAndCol
          )

          loweredTargetAssignNodes ++ suite
        case None =>
          suite
      }

    // TODO For the except handler we currently lower as:
    //   hit_except = True
    //   exit(manager)
    // instead of:
    //   hit_except = True
    //   if not exit(manager, *sys.exc_info()):
    //     raise

    val finalBlockStmts =
      createInstanceCall(
        createIdentifierNode("__exit__", Load, lineAndCol),
        createIdentifierNode(managerIdentifierName, Load, lineAndCol),
        "",
        lineAndCol,
        Nil,
        Nil
      ) :: Nil

    val tryBlock = createTry(tryBody, Nil, finalBlockStmts, Nil, lineAndCol)

    val blockStmts = mutable.ArrayBuffer.empty[nodes.NewNode]
    blockStmts.append(assignmentToManager)
    blockStmts.append(assignmentToEnter)
    blockStmts.append(assignmentToExit)
    blockStmts.append(assignmentToValue)
    blockStmts.append(tryBlock)

    createBlock(blockStmts, lineAndCol)
  }

  // TODO add case pattern and guard statements to cpg
  def convert(matchStmt: ast.Match): NewNode = {
    val controlStructureNode =
      nodeBuilder.controlStructureNode("match ... : ...", ControlStructureTypes.SWITCH, lineAndColOf(matchStmt))

    val matchSubject = convert(matchStmt.subject)

    val caseBlocks = matchStmt.cases.map { caseStmt =>
      val bodyNodes = caseStmt.body.map(convert)
      createBlock(bodyNodes, lineAndColOf(caseStmt.pattern))
    }

    edgeBuilder.conditionEdge(matchSubject, controlStructureNode)
    addAstChildNodes(controlStructureNode, 1, matchSubject)
    addAstChildNodes(controlStructureNode, 2, caseBlocks)

    controlStructureNode
  }

  def convert(raise: ast.Raise): NewNode = {
    val excNodeOption   = raise.exc.map(convert)
    val causeNodeOption = raise.cause.map(convert)

    val args = mutable.ArrayBuffer.empty[nodes.NewNode]
    args.appendAll(excNodeOption)
    args.appendAll(causeNodeOption)

    val code = "raise" +
      excNodeOption.map(excNode => " " + codeOf(excNode)).getOrElse("") +
      causeNodeOption.map(causeNode => " from " + codeOf(causeNode)).getOrElse("")

    val callNode = nodeBuilder.callNode(code, "<operator>.raise", DispatchTypes.STATIC_DISPATCH, lineAndColOf(raise))

    addAstChildrenAsArguments(callNode, 1, args)

    callNode
  }

  def convert(tryStmt: ast.Try): NewNode = {
    createTry(
      tryStmt.body.map(convert),
      tryStmt.handlers.map(convert),
      tryStmt.finalbody.map(convert),
      tryStmt.orelse.map(convert),
      lineAndColOf(tryStmt)
    )
  }

  def convert(assert: ast.Assert): NewNode = {
    val testNode = convert(assert.test)
    val msgNode  = assert.msg.map(convert)

    val code     = "assert " + codeOf(testNode) + msgNode.map(m => ", " + codeOf(m)).getOrElse("")
    val callNode = nodeBuilder.callNode(code, "<operator>.assert", DispatchTypes.STATIC_DISPATCH, lineAndColOf(assert))

    addAstChildrenAsArguments(callNode, 1, testNode)
    if (msgNode.isDefined) {
      addAstChildrenAsArguments(callNode, 2, msgNode)
    }
    callNode
  }

  // Lowering of import x:
  //   x = import("", "x")
  // Lowering of import x as y:
  //   y = import("", "x")
  // Lowering of import x, y:
  //   {
  //     x = import("", "x")
  //     y = import("", "y")
  //   }
  def convert(importStmt: ast.Import): NewNode = {
    createTransformedImport("", importStmt.names, lineAndColOf(importStmt))
  }

  // Lowering of from x import y:
  //   y = import("x", "y")
  // Lowering of from x import y as z:
  //   z = import("x", "y")
  // Lowering of from x import y, z:
  //   {
  //     y = import("x", "y")
  //     z = import("x", "z")
  //   }
  def convert(importFrom: ast.ImportFrom): NewNode = {
    var moduleName = ""

    for (i <- 0 until importFrom.level) {
      moduleName = moduleName.appended('.')
    }
    moduleName += importFrom.module.getOrElse("")

    createTransformedImport(moduleName, importFrom.names, lineAndColOf(importFrom))
  }

  def convert(global: ast.Global): NewNode = {
    global.names.foreach(contextStack.addGlobalVariable)
    val code = global.names.mkString("global ", ", ", "")
    nodeBuilder.unknownNode(code, global.getClass.getName, lineAndColOf(global))
  }

  def convert(nonLocal: ast.Nonlocal): NewNode = {
    nonLocal.names.foreach(contextStack.addNonLocalVariable)
    val code = nonLocal.names.mkString("nonlocal ", ", ", "")
    nodeBuilder.unknownNode(code, nonLocal.getClass.getName, lineAndColOf(nonLocal))
  }

  def convert(expr: ast.Expr): nodes.NewNode = {
    convert(expr.value)
  }

  def convert(pass: ast.Pass): nodes.NewNode = {
    nodeBuilder.callNode("pass", "<operator>.pass", DispatchTypes.STATIC_DISPATCH, lineAndColOf(pass))
  }

  def convert(astBreak: ast.Break): nodes.NewNode = {
    nodeBuilder.controlStructureNode("break", ControlStructureTypes.BREAK, lineAndColOf(astBreak))
  }

  def convert(astContinue: ast.Continue): nodes.NewNode = {
    nodeBuilder.controlStructureNode("continue", ControlStructureTypes.CONTINUE, lineAndColOf(astContinue))
  }

  def convert(raise: ast.RaiseP2): NewNode = ???

  def convert(errorStatement: ast.ErrorStatement): NewNode = {
    val code   = nodeToCode.getCode(errorStatement)
    val line   = errorStatement.attributeProvider.lineno
    val column = errorStatement.attributeProvider.col_offset
    logger.warn(
      s"Could not parse file $relFileName at line $line column $column. Invalid code: $code" +
        s"\nParser exception message: ${errorStatement.exception.getMessage}"
    )
    nodeBuilder.unknownNode(errorStatement.toString, errorStatement.getClass.getName, lineAndColOf(errorStatement))
  }

  def convert(expr: ast.iexpr): NewNode = {
    expr match {
      case node: ast.BoolOp         => convert(node)
      case node: ast.NamedExpr      => convert(node)
      case node: ast.BinOp          => convert(node)
      case node: ast.UnaryOp        => convert(node)
      case node: ast.Lambda         => convert(node)
      case node: ast.IfExp          => convert(node)
      case node: ast.Dict           => convert(node)
      case node: ast.Set            => convert(node)
      case node: ast.ListComp       => convert(node)
      case node: ast.SetComp        => convert(node)
      case node: ast.DictComp       => convert(node)
      case node: ast.GeneratorExp   => convert(node)
      case node: ast.Await          => convert(node)
      case node: ast.Yield          => unhandled(node)
      case node: ast.YieldFrom      => unhandled(node)
      case node: ast.Compare        => convert(node)
      case node: ast.Call           => convert(node)
      case node: ast.FormattedValue => convert(node)
      case node: ast.JoinedString   => convert(node)
      case node: ast.Constant       => convert(node)
      case node: ast.Attribute      => convert(node)
      case node: ast.Subscript      => convert(node)
      case node: ast.Starred        => convert(node)
      case node: ast.Name           => convert(node)
      case node: ast.List           => convert(node)
      case node: ast.Tuple          => convert(node)
      case node: ast.Slice          =>
        // Our expectation is that ast.Slice only appears as part of ast.Subscript
        // and thus we should never get here becauase convert(ast.Subscript)
        // directly handles the case of a nested ast.Slice.
        unhandled(node)
      case node: ast.StringExpList => convert(node)
    }
  }

  def convert(boolOp: ast.BoolOp): nodes.NewNode = {
    def boolOpToCodeAndFullName(operator: ast.iboolop): () => (String, String) = { () =>
      {
        operator match {
          case ast.And => ("and", Operators.logicalAnd)
          case ast.Or  => ("or", Operators.logicalOr)
        }
      }
    }

    val operandNodes = boolOp.values.map(convert)
    createNAryOperatorCall(boolOpToCodeAndFullName(boolOp.op), operandNodes, lineAndColOf(boolOp))
  }

  // TODO test
  def convert(namedExpr: ast.NamedExpr): NewNode = {
    val targetNode = convert(namedExpr.target)
    val valueNode  = convert(namedExpr.value)

    createAssignment(targetNode, valueNode, lineAndColOf(namedExpr))
  }

  def convert(binOp: ast.BinOp): nodes.NewNode = {
    val lhsNode = convert(binOp.left)
    val rhsNode = convert(binOp.right)

    val opCodeAndFullName =
      binOp.op match {
        case ast.Add  => ("+", Operators.addition)
        case ast.Sub  => ("-", Operators.subtraction)
        case ast.Mult => ("*", Operators.multiplication)
        case ast.MatMult =>
          ("@", "<operator>.matMult") // TODO make this a define and add policy for this
        case ast.Div    => ("/", Operators.division)
        case ast.Mod    => ("%", Operators.modulo)
        case ast.Pow    => ("**", Operators.exponentiation)
        case ast.LShift => ("<<", Operators.shiftLeft)
        case ast.RShift => (">>", Operators.arithmeticShiftRight)
        case ast.BitOr  => ("|", Operators.or)
        case ast.BitXor => ("^", Operators.xor)
        case ast.BitAnd => ("&", Operators.and)
        case ast.FloorDiv =>
          ("//", "<operator>.floorDiv") // TODO make this a define and add policy for this
      }

    createBinaryOperatorCall(lhsNode, () => opCodeAndFullName, rhsNode, lineAndColOf(binOp))
  }

  def convert(unaryOp: ast.UnaryOp): nodes.NewNode = {
    val operandNode = convert(unaryOp.operand)

    val (operatorCode, methodFullName) =
      unaryOp.op match {
        case ast.Invert => ("~", Operators.not)
        case ast.Not    => ("not ", Operators.logicalNot)
        case ast.UAdd   => ("+", Operators.plus)
        case ast.USub   => ("-", Operators.minus)
      }

    val code     = operatorCode + codeOf(operandNode)
    val callNode = nodeBuilder.callNode(code, methodFullName, DispatchTypes.STATIC_DISPATCH, lineAndColOf(unaryOp))

    addAstChildrenAsArguments(callNode, 1, operandNode)

    callNode
  }

  def convert(lambda: ast.Lambda): NewNode = {
    // TODO test lambda expression.
    val lambdaCounter = contextStack.getAndIncLambdaCounter()
    val lambdaNumberSuffix =
      if (lambdaCounter == 0) {
        ""
      } else {
        lambdaCounter.toString
      }

    val name = nextClosureName()
    val (_, methodRefNode) = createMethodAndMethodRef(
      name,
      Some(name),
      createParameterProcessingFunction(lambda.args, isStatic = false),
      () => Iterable.single(convert(new ast.Return(lambda.body, lambda.attributeProvider))),
      returns = None,
      isAsync = false,
      lineAndColOf(lambda),
      ModifierTypes.LAMBDA :: Nil
    )
    methodRefNode
  }

  // TODO test
  def convert(ifExp: ast.IfExp): NewNode = {
    val bodyNode   = convert(ifExp.body)
    val testNode   = convert(ifExp.test)
    val orElseNode = convert(ifExp.orelse)

    val code     = codeOf(bodyNode) + " if " + codeOf(testNode) + " else " + codeOf(orElseNode)
    val callNode = nodeBuilder.callNode(code, Operators.conditional, DispatchTypes.STATIC_DISPATCH, lineAndColOf(ifExp))

    // testNode is first argument to match semantics of Operators.conditional.
    addAstChildrenAsArguments(callNode, 1, testNode, bodyNode, orElseNode)

    callNode
  }

  /** Lowering of {x:1, y:2, **z}: { tmp = {} tmp[x] = 1 tmp[y] = 2 tmp.update(z) tmp }
    */
  // TODO test
  def convert(dict: ast.Dict): NewNode = {
    val MAX_KV_PAIRS    = 1000
    val tmpVariableName = getUnusedName()
    val dictOperatorCall =
      createLiteralOperatorCall("{", "}", "<operator>.dictLiteral", lineAndColOf(dict))
    val dictVariableAssigNode =
      createAssignmentToIdentifier(tmpVariableName, dictOperatorCall, lineAndColOf(dict))

    val dictElementAssignNodes = if (dict.keys.size > MAX_KV_PAIRS) {
      Seq(
        nodeBuilder
          .callNode("<too-many-key-value-pairs>", Constants.ANY, DispatchTypes.STATIC_DISPATCH, lineAndColOf(dict))
      )
    } else {
      dict.keys.zip(dict.values).map { case (key, value) =>
        key match {
          case Some(key) =>
            val indexAccessNode = createIndexAccess(
              createIdentifierNode(tmpVariableName, Load, lineAndColOf(dict)),
              convert(key),
              lineAndColOf(dict)
            )

            createAssignment(indexAccessNode, convert(value), lineAndColOf(dict))
          case None =>
            createXDotYCall(
              () => createIdentifierNode(tmpVariableName, Load, lineAndColOf(dict)),
              "update",
              xMayHaveSideEffects = false,
              lineAndColOf(dict),
              convert(value) :: Nil,
              Nil
            )
        }
      }
    }

    val dictInstanceReturnIdentifierNode =
      createIdentifierNode(tmpVariableName, Load, lineAndColOf(dict))

    val blockElements = mutable.ArrayBuffer.empty[nodes.NewNode]
    blockElements.append(dictVariableAssigNode)
    blockElements.appendAll(dictElementAssignNodes)
    blockElements.append(dictInstanceReturnIdentifierNode)
    createBlock(blockElements, lineAndColOf(dict))
  }

  // TODO test
  def convert(set: ast.Set): nodes.NewNode = {
    val setElementNodes = set.elts.map(convert)
    val code            = setElementNodes.map(codeOf).mkString("{", ", ", "}")

    val callNode = nodeBuilder.callNode(code, "<operator>.setLiteral", DispatchTypes.STATIC_DISPATCH, lineAndColOf(set))

    addAstChildrenAsArguments(callNode, 1, setElementNodes)

    callNode
  }

  /** Lowering of [x for y in l for x in y]: { tmp = [] <loweringOf>( for y in l: for x in y: tmp.append(x) ) tmp }
    */
  // TODO test
  def convert(listComp: ast.ListComp): NewNode = {
    contextStack.pushSpecialContext()
    val tmpVariableName = getUnusedName()

    // Create tmp = list()
    val listOperatorCall =
      createLiteralOperatorCall("[", "]", "<operator>.listLiteral", lineAndColOf(listComp))
    val variableAssignNode =
      createAssignmentToIdentifier(tmpVariableName, listOperatorCall, lineAndColOf(listComp))

    // Create tmp.append(x)
    val listVarAppendCallNode = createXDotYCall(
      () => createIdentifierNode(tmpVariableName, Load, lineAndColOf(listComp)),
      "append",
      xMayHaveSideEffects = false,
      lineAndColOf(listComp),
      convert(listComp.elt) :: Nil,
      Nil
    )

    val comprehensionBlockNode = createComprehensionLowering(
      tmpVariableName,
      variableAssignNode,
      listVarAppendCallNode,
      listComp.generators,
      lineAndColOf(listComp)
    )

    contextStack.pop()

    comprehensionBlockNode
  }

  /** Lowering of {x for y in l for x in y}: { tmp = {} <loweringOf>( for y in l: for x in y: tmp.add(x) ) tmp }
    */
  // TODO test
  def convert(setComp: ast.SetComp): NewNode = {
    contextStack.pushSpecialContext()
    val tmpVariableName = getUnusedName()

    val setOperatorCall =
      createLiteralOperatorCall("{", "}", "<operator>.setLiteral", lineAndColOf(setComp))
    val variableAssignNode =
      createAssignmentToIdentifier(tmpVariableName, setOperatorCall, lineAndColOf(setComp))

    // Create tmp.add(x)
    val setVarAddCallNode = createXDotYCall(
      () => createIdentifierNode(tmpVariableName, Load, lineAndColOf(setComp)),
      "add",
      xMayHaveSideEffects = false,
      lineAndColOf(setComp),
      convert(setComp.elt) :: Nil,
      Nil
    )

    val comprehensionBlockNode = createComprehensionLowering(
      tmpVariableName,
      variableAssignNode,
      setVarAddCallNode,
      setComp.generators,
      lineAndColOf(setComp)
    )

    contextStack.pop()

    comprehensionBlockNode
  }

  /** Lowering of {k:v for y in l for k, v in y}: { tmp = {} <loweringOf>( for y in l: for k, v in y: tmp[k] = v ) tmp }
    */
  // TODO test
  def convert(dictComp: ast.DictComp): NewNode = {
    contextStack.pushSpecialContext()
    val tmpVariableName = getUnusedName()

    val dictOperatorCall =
      createLiteralOperatorCall("{", "}", "<operator>.dictLiteral", lineAndColOf(dictComp))
    val variableAssignNode =
      createAssignmentToIdentifier(tmpVariableName, dictOperatorCall, lineAndColOf(dictComp))

    // Create tmp[k] = v
    val dictAssigNode = createAssignment(
      createIndexAccess(
        createIdentifierNode(tmpVariableName, Load, lineAndColOf(dictComp)),
        convert(dictComp.key),
        lineAndColOf(dictComp)
      ),
      convert(dictComp.value),
      lineAndColOf(dictComp)
    )

    val comprehensionBlockNode = createComprehensionLowering(
      tmpVariableName,
      variableAssignNode,
      dictAssigNode,
      dictComp.generators,
      lineAndColOf(dictComp)
    )

    contextStack.pop()

    comprehensionBlockNode
  }

  /** Lowering of (x for y in l for x in y): { tmp = <operator>.genExp <loweringOf>( for y in l: for x in y:
    * tmp.append(x) ) tmp } This lowering is not quite correct as it ignores the lazy evaluation of the generator
    * expression. Instead it just mimics the list comprehension lowering but for now this is good enough.
    */
  // TODO test
  def convert(generatorExp: ast.GeneratorExp): NewNode = {
    contextStack.pushSpecialContext()
    val tmpVariableName = getUnusedName()

    // Create tmp = list()
    val genExpOperatorCall =
      nodeBuilder.callNode(
        "<operator>.genExp",
        "<operator>.genExp",
        DispatchTypes.STATIC_DISPATCH,
        lineAndColOf(generatorExp)
      )

    val variableAssignNode =
      createAssignmentToIdentifier(tmpVariableName, genExpOperatorCall, lineAndColOf(generatorExp))

    // Create tmp.append(x)
    val genExpAppendCallNode = createXDotYCall(
      () => createIdentifierNode(tmpVariableName, Load, lineAndColOf(generatorExp)),
      "append",
      xMayHaveSideEffects = false,
      lineAndColOf(generatorExp),
      convert(generatorExp.elt) :: Nil,
      Nil
    )

    val comprehensionBlockNode = createComprehensionLowering(
      tmpVariableName,
      variableAssignNode,
      genExpAppendCallNode,
      generatorExp.generators,
      lineAndColOf(generatorExp)
    )

    contextStack.pop()

    comprehensionBlockNode
  }

  def convert(await: ast.Await): NewNode = {
    // Since the CPG format does not provide means to model async/await,
    // we for now treat it as non existing.
    convert(await.value)
  }

  def convert(yieldExpr: ast.Yield): NewNode = ???

  def convert(yieldFrom: ast.YieldFrom): NewNode = ???

  // In case of a single compare operation there is no lowering applied.
  // So e.g. x < y stay untouched.
  // Otherwise the lowering is as follows:
  //  Src AST:
  //    x < y < z < a
  //  Lowering:
  //    {
  //      tmp1 = y
  //      x < tmp1 && {
  //        tmp2 = z
  //        tmp1 < tmp2 && {
  //          tmp2 < a
  //        }
  //      }
  //    }
  def convert(compare: ast.Compare): NewNode = {
    assert(compare.ops.size == compare.comparators.size)
    var lhsNode = convert(compare.left)

    val topLevelExprNodes =
      lowerComparatorChain(lhsNode, compare.ops, compare.comparators, lineAndColOf(compare))
    if (topLevelExprNodes.size > 1) {
      createBlock(topLevelExprNodes, lineAndColOf(compare))
    } else {
      topLevelExprNodes.head
    }
  }

  private def compopToOpCodeAndFullName(compareOp: ast.icompop): () => (String, String) = { () =>
    {
      compareOp match {
        case ast.Eq    => ("==", Operators.equals)
        case ast.NotEq => ("!=", Operators.notEquals)
        case ast.Lt    => ("<", Operators.lessThan)
        case ast.LtE   => ("<=", Operators.lessEqualsThan)
        case ast.Gt    => (">", Operators.greaterThan)
        case ast.GtE   => (">=", Operators.greaterEqualsThan)
        case ast.Is    => ("is", "<operator>.is")
        case ast.IsNot => ("is not", "<operator>.isNot")
        case ast.In    => ("in", "<operator>.in")
        case ast.NotIn => ("not in", "<operator>.notIn")
      }
    }
  }

  def lowerComparatorChain(
    lhsNode: nodes.NewNode,
    compOperators: Iterable[ast.icompop],
    comparators: Iterable[ast.iexpr],
    lineAndColumn: LineAndColumn
  ): Iterable[nodes.NewNode] = {
    val rhsNode = convert(comparators.head)

    if (compOperators.size == 1) {
      val compareNode =
        createBinaryOperatorCall(lhsNode, compopToOpCodeAndFullName(compOperators.head), rhsNode, lineAndColumn)
      Iterable.single(compareNode)
    } else {
      val tmpVariableName = getUnusedName()
      val assignmentNode  = createAssignmentToIdentifier(tmpVariableName, rhsNode, lineAndColumn)

      val tmpIdentifierCompare1 = createIdentifierNode(tmpVariableName, Load, lineAndColumn)
      val compareNode = createBinaryOperatorCall(
        lhsNode,
        compopToOpCodeAndFullName(compOperators.head),
        tmpIdentifierCompare1,
        lineAndColumn
      )

      val tmpIdentifierCompare2 = createIdentifierNode(tmpVariableName, Load, lineAndColumn)
      val childNodes = lowerComparatorChain(tmpIdentifierCompare2, compOperators.tail, comparators.tail, lineAndColumn)

      val blockNode = createBlock(childNodes, lineAndColumn)

      Iterable(assignmentNode, createBinaryOperatorCall(compareNode, andOpCodeAndFullName(), blockNode, lineAndColumn))
    }
  }

  private def andOpCodeAndFullName(): () => (String, String) = { () =>
    ("and", Operators.logicalAnd)
  }

  /** TODO For now this function compromises on the correctness of the lowering in order to get some data flow tracking
    * going.
    *   1. For constructs like x.func() we assume x to be the instance which is passed into func. This is not true since
    *      the instance method object gets the instance already bound/captured during function access. This becomes
    *      relevant for constructs like: x.func = y.func <- y.func is class method object x.func() In this case the
    *      instance passed into func is y and not x. We cannot represent this in th CPG and thus stick to the assumption
    *      that the part before the "." and the bound/captured instance will be the same. For reference see:
    *      https://docs.python.org/3/reference/datamodel.html#the-standard-type-hierarchy search for "Instance methods"
    */
  def convert(call: ast.Call): nodes.NewNode = {
    val argumentNodes = call.args.map(convert).toSeq
    val keywordArgNodes = call.keywords.flatMap { keyword =>
      if (keyword.arg.isDefined) {
        Some((keyword.arg.get, convert(keyword.value)))
      } else {
        // keyword.arg == None. This is the case for func(**dict) style arguments.
        // TODO implement handling for this case.
        None
      }
    }

    call.func match {
      case attribute: ast.Attribute =>
        createXDotYCall(
          () => convert(attribute.value),
          attribute.attr,
          xMayHaveSideEffects = !attribute.value.isInstanceOf[ast.Name],
          lineAndColOf(call),
          argumentNodes,
          keywordArgNodes
        )
      case _ =>
        val receiverNode = convert(call.func)
        val name = call.func match {
          case ast.Name(id, _) => id
          case _               => ""
        }
        createCall(receiverNode, name, lineAndColOf(call), argumentNodes, keywordArgNodes)
    }
  }

  def convert(formattedValue: ast.FormattedValue): nodes.NewNode = {
    val valueNode = convert(formattedValue.value)

    val equalSignStr = if (formattedValue.equalSign) "=" else ""
    val conversionStr = formattedValue.conversion match {
      case -1  => ""
      case 115 => "!s"
      case 114 => "!r"
      case 97  => "!a"
    }

    val formatSpecStr = formattedValue.format_spec match {
      case Some(formatSpec) => ":" + formatSpec
      case None             => ""
    }

    val code = "{" + codeOf(valueNode) + equalSignStr + conversionStr + formatSpecStr + "}"

    val callNode = nodeBuilder.callNode(
      code,
      "<operator>.formattedValue",
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(formattedValue)
    )

    addAstChildrenAsArguments(callNode, 1, valueNode)

    callNode
  }

  def convert(joinedString: ast.JoinedString): nodes.NewNode = {
    val argumentNodes = joinedString.values.map(convert)

    val code = joinedString.prefix + joinedString.quote + argumentNodes
      .map(codeOf)
      .mkString("") + joinedString.quote

    val callNode =
      nodeBuilder.callNode(code, "<operator>.formatString", DispatchTypes.STATIC_DISPATCH, lineAndColOf(joinedString))

    addAstChildrenAsArguments(callNode, 1, argumentNodes)

    callNode
  }

  def convert(constant: ast.Constant): nodes.NewNode = {
    constant.value match {
      case stringConstant: ast.StringConstant =>
        if (stringConstant.prefix.contains("b") || stringConstant.prefix.contains("B")) {
          nodeBuilder.bytesLiteralNode(
            stringConstant.prefix + stringConstant.quote + stringConstant.value + stringConstant.quote,
            lineAndColOf(constant)
          )
        } else {
          nodeBuilder.stringLiteralNode(
            stringConstant.prefix + stringConstant.quote + stringConstant.value + stringConstant.quote,
            lineAndColOf(constant)
          )
        }
      case stringConstant: ast.JoinedStringConstant =>
        nodeBuilder.stringLiteralNode(stringConstant.value, lineAndColOf(constant))
      case boolConstant: ast.BoolConstant =>
        val boolStr = if (boolConstant.value) "True" else "False"
        nodeBuilder.intLiteralNode(boolStr, lineAndColOf(constant))
      case intConstant: ast.IntConstant =>
        nodeBuilder.intLiteralNode(intConstant.value, lineAndColOf(constant))
      case floatConstant: ast.FloatConstant =>
        nodeBuilder.floatLiteralNode(floatConstant.value, lineAndColOf(constant))
      case imaginaryConstant: ast.ImaginaryConstant =>
        nodeBuilder.complexLiteralNode(imaginaryConstant.value + "j", lineAndColOf(constant))
      case ast.NoneConstant =>
        nodeBuilder.literalNode("None", None, lineAndColOf(constant))
      case ast.EllipsisConstant =>
        nodeBuilder.literalNode("...", None, lineAndColOf(constant))
    }
  }

  /** TODO We currently ignore possible attribute access provider/interception mechanisms like __getattr__,
    * __getattribute__ and __get__.
    */
  def convert(attribute: ast.Attribute): nodes.NewNode = {
    val baseNode   = convert(attribute.value)
    val fieldName  = attribute.attr
    val lineAndCol = lineAndColOf(attribute)

    val fieldAccess = createFieldAccess(baseNode, fieldName, lineAndCol)

    attribute.value match {
      case name: ast.Name if name.id == "self" =>
        createAndRegisterMember(fieldName, lineAndCol)
      case _ =>
    }

    fieldAccess
  }

  private def createAndRegisterMember(name: String, lineAndCol: LineAndColumn): Unit = {
    contextStack.findEnclosingTypeDecl() match {
      case Some(typeDecl: NewTypeDecl) =>
        if (!members.contains(typeDecl) || !members(typeDecl).contains(name)) {
          val member = nodeBuilder.memberNode(name, lineAndCol)
          edgeBuilder.astEdge(member, typeDecl, contextStack.order.getAndInc)
          members(typeDecl) = members.getOrElse(typeDecl, List()) ++ List(name)
        }
      case _ =>
    }
  }

  def convert(subscript: ast.Subscript): NewNode = {
    subscript.slice match {
      case slice: ast.Slice =>
        val value = convert(subscript.value)
        val lower = slice.lower.map(convert).getOrElse(nodeBuilder.literalNode("None", None, noLineAndColumn))
        val upper = slice.upper.map(convert).getOrElse(nodeBuilder.literalNode("None", None, noLineAndColumn))
        val step  = slice.step.map(convert).getOrElse(nodeBuilder.literalNode("None", None, noLineAndColumn))

        val code = nodeToCode.getCode(subscript)
        val callNode =
          nodeBuilder.callNode(code, "<operator>.slice", DispatchTypes.STATIC_DISPATCH, lineAndColOf(slice))

        val args = value :: lower :: upper :: step :: Nil
        addAstChildrenAsArguments(callNode, 1, args)

        callNode
      case _ =>
        createIndexAccess(convert(subscript.value), convert(subscript.slice), lineAndColOf(subscript))
    }
  }

  def convert(starred: ast.Starred): NewNode = {
    val memoryOperation = memOpMap.get(starred).get

    memoryOperation match {
      case Load =>
        val unrollOperand = convert(starred.value)
        createStarredUnpackOperatorCall(unrollOperand, lineAndColOf(starred))
      case Store =>
        unhandled(starred)
      case Del =>
        // This case is not possible since star operator is not allowed in delete statement.
        unhandled(starred)
    }
  }

  def convert(name: ast.Name): nodes.NewNode = {
    val memoryOperation = memOpMap.get(name).get
    val identifier      = createIdentifierNode(name.id, memoryOperation, lineAndColOf(name))
    if (contextStack.isClassContext && memoryOperation == Store) {
      createAndRegisterMember(identifier.name, lineAndColOf(name))
    }
    identifier
  }

  // TODO test
  def convert(list: ast.List): nodes.NewNode = {
    // Must be a List as part of a Load memory operation because a List literal
    // is not permitted as argument to a Del and List as part of a Store does not
    // reach here.
    assert(memOpMap.get(list).get == Load)
    val listElementNodes = list.elts.map(convert)
    val code             = listElementNodes.map(codeOf).mkString("[", ", ", "]")

    val callNode =
      nodeBuilder.callNode(code, "<operator>.listLiteral", DispatchTypes.STATIC_DISPATCH, lineAndColOf(list))

    addAstChildrenAsArguments(callNode, 1, listElementNodes)

    callNode
  }

  // TODO test
  def convert(tuple: ast.Tuple): NewNode = {
    // Must be a tuple as part of a Load or Del memory operation because Tuples in
    // store contexts are not supposed to reach here. They need to be lowered by
    // createValueToTargetsDecomposition.
    assert(memOpMap.get(tuple).get == Load || memOpMap.get(tuple).get == Del)
    val tupleElementNodes = tuple.elts.map(convert)
    val code = if (tupleElementNodes.size != 1) {
      tupleElementNodes.map(codeOf).mkString("(", ", ", ")")
    } else {
      "(" + codeOf(tupleElementNodes.head) + ",)"
    }

    val callNode =
      nodeBuilder.callNode(code, "<operator>.tupleLiteral", DispatchTypes.STATIC_DISPATCH, lineAndColOf(tuple))

    addAstChildrenAsArguments(callNode, 1, tupleElementNodes)

    callNode
  }

  def convert(slice: ast.Slice): NewNode = {
    val args = mutable.ArrayBuffer.empty[NewNode]
    slice.lower.foreach(expr => args.append(convert(expr)))
    slice.upper.foreach(expr => args.append(convert(expr)))
    slice.step.foreach(expr => args.append(convert(expr)))

    val code     = nodeToCode.getCode(slice)
    val callNode = nodeBuilder.callNode(code, "<operator>.slice", DispatchTypes.STATIC_DISPATCH, lineAndColOf(slice))

    addAstChildrenAsArguments(callNode, 1, args)

    callNode
  }

  def convert(stringExpList: ast.StringExpList): NewNode = {
    val stringNodes = stringExpList.elts.map(convert)
    val code        = stringNodes.map(codeOf).mkString(" ")

    val callNode = nodeBuilder.callNode(
      code,
      "<operator>.stringExpressionList",
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(stringExpList)
    )

    addAstChildrenAsArguments(callNode, 1, stringNodes)

    callNode
  }

  // TODO Since there is now real concept of reflecting exception handlers
  // semantically in the CPG we just make sure that the variable scoping
  // is right and that we convert the exception handler body.
  // TODO tests
  def convert(exceptHandler: ast.ExceptHandler): NewNode = {
    contextStack.pushSpecialContext()
    val specialTargetLocals = mutable.ArrayBuffer.empty[nodes.NewLocal]
    if (exceptHandler.name.isDefined) {
      val localNode = nodeBuilder.localNode(exceptHandler.name.get, None)
      specialTargetLocals.append(localNode)
      contextStack.addSpecialVariable(localNode)
    }

    val blockNode = createBlock(exceptHandler.body.map(convert), lineAndColOf(exceptHandler))
    addAstChildNodes(blockNode, 1, specialTargetLocals)

    contextStack.pop()

    blockNode
  }

  def convert(parameters: ast.Arguments, startIndex: Int): Iterable[nodes.NewMethodParameterIn] = {
    val autoIncIndex = new AutoIncIndex(startIndex)

    parameters.posonlyargs.map(convertPosOnlyArg(_, autoIncIndex)) ++
      parameters.args.map(convertNormalArg(_, autoIncIndex)) ++
      parameters.vararg.map(convertVarArg(_, autoIncIndex)) ++
      parameters.kwonlyargs.map(convertKeywordOnlyArg) ++
      parameters.kw_arg.map(convertKwArg)
  }

  // TODO for now the different arg convert functions are all the same but
  // will all be slightly different in the future when we can represent the
  // different types in the cpg.
  def convertPosOnlyArg(arg: ast.Arg, index: AutoIncIndex): nodes.NewMethodParameterIn = {
    nodeBuilder.methodParameterNode(
      arg.arg,
      isVariadic = false,
      lineAndColOf(arg),
      Option(index.getAndInc),
      arg.annotation
    )
  }

  def convertNormalArg(arg: ast.Arg, index: AutoIncIndex): nodes.NewMethodParameterIn = {
    nodeBuilder.methodParameterNode(
      arg.arg,
      isVariadic = false,
      lineAndColOf(arg),
      Option(index.getAndInc),
      arg.annotation
    )
  }

  def convertVarArg(arg: ast.Arg, index: AutoIncIndex): nodes.NewMethodParameterIn = {
    nodeBuilder.methodParameterNode(arg.arg, isVariadic = true, lineAndColOf(arg), Option(index.getAndInc))
  }

  def convertKeywordOnlyArg(arg: ast.Arg): nodes.NewMethodParameterIn = {
    nodeBuilder.methodParameterNode(arg.arg, isVariadic = false, lineAndColOf(arg))
  }

  def convertKwArg(arg: ast.Arg): nodes.NewMethodParameterIn = {
    nodeBuilder.methodParameterNode(arg.arg, isVariadic = false, lineAndColOf(arg))
  }

  def convert(keyword: ast.Keyword): NewNode = ???

  def convert(alias: ast.Alias): NewNode = ???

  def convert(typeIgnore: ast.TypeIgnore): NewNode = ???

  private def calculateFullNameFromContext(name: String): String = {
    val contextQualName = contextStack.qualName
    if (contextQualName != "") {
      relFileName + ":" + contextQualName + "." + name
    } else {
      relFileName + ":" + name
    }
  }
}

object PythonAstVisitor {
  private val logger = LoggerFactory.getLogger(getClass)

  val typingPrefix    = "typing."
  val metaClassSuffix = "<meta>"

  val noLineAndColumn = LineAndColumn(-1, -1, -1, -1, -1, -1)

  // This list contains all functions from https://docs.python.org/3/library/functions.html#built-in-funcs
  // for python version 3.9.5.
  // There is a corresponding list in policies which needs to be updated if this one is updated and vice versa.
  val builtinFunctionsV3: Iterable[String] = Iterable(
    "abs",
    "all",
    "any",
    "ascii",
    "bin",
    "breakpoint",
    "callable",
    "chr",
    "classmethod",
    "compile",
    "delattr",
    "dir",
    "divmod",
    "enumerate",
    "eval",
    "exec",
    "filter",
    "format",
    "getattr",
    "globals",
    "hasattr",
    "hash",
    "help",
    "hex",
    "id",
    "input",
    "isinstance",
    "issubclass",
    "iter",
    "len",
    "locals",
    "map",
    "max",
    "min",
    "next",
    "oct",
    "open",
    "ord",
    "pow",
    "print",
    "repr",
    "reversed",
    "round",
    "setattr",
    "sorted",
    "staticmethod",
    "sum",
    "super",
    "vars",
    "zip",
    "__import__"
  )
  // This list contains all classes from https://docs.python.org/3/library/functions.html#built-in-funcs
  // for python version 3.9.5.
  val builtinClassesV3: Iterable[String] = Iterable(
    "bool",
    "bytearray",
    "bytes",
    "complex",
    "dict",
    "float",
    "frozenset",
    "int",
    "list",
    "memoryview",
    "object",
    "property",
    "range",
    "set",
    "slice",
    "str",
    "tuple",
    "type"
  )
  // This list contains all functions from https://docs.python.org/2.7/library/functions.html
  val builtinFunctionsV2: Iterable[String] = Iterable(
    "abs",
    "all",
    "any",
    "bin",
    "callable",
    "chr",
    "classmethod",
    "cmp",
    "compile",
    "delattr",
    "dir",
    "divmod",
    "enumerate",
    "eval",
    // This one is special because it is not from the above mentioned list.
    // This is because exec is a statement type in V2 but our parser provides
    // it to us as a normal call so that we can model it as builtin.
    "exec",
    "execfile",
    "filter",
    "format",
    "getattr",
    "globals",
    "hasattr",
    "hash",
    "help",
    "hex",
    "id",
    "input",
    "isinstance",
    "issubclass",
    "iter",
    "len",
    "locals",
    "map",
    "max",
    "min",
    "next",
    "oct",
    "open",
    "ord",
    "pow",
    "print",
    "range",
    "raw_input",
    "reduce",
    "reload",
    "repr",
    "reversed",
    "round",
    "setattr",
    "sorted",
    "staticmethod",
    "sum",
    "super",
    "unichr",
    "vars",
    "zip",
    "__import__"
  )
  // This list contains all classes from https://docs.python.org/2.7/library/functions.html
  val builtinClassesV2: Iterable[String] = Iterable(
    "bool",
    "bytearray",
    "complex",
    "dict",
    "file",
    "float",
    "frozenset",
    "int",
    "list",
    "long",
    "memoryview",
    "object",
    "property",
    "set",
    "slice",
    "str",
    "tuple",
    "type",
    "unicode",
    "xrange"
  )

  lazy val allBuiltinClasses: Set[String] = (builtinClassesV2 ++ builtinClassesV3).toSet

  lazy val typingClassesV3: Set[String] = Set(
    "Annotated",
    "Any",
    "Callable",
    "ClassVar",
    "Final",
    "ForwardRef",
    "Generic",
    "Literal",
    "Optional",
    "Protocol",
    "Tuple",
    "Type",
    "TypeVar",
    "Union",
    "AbstractSet",
    "ByteString",
    "Container",
    "ContextManager",
    "Hashable",
    "ItemsView",
    "Iterable",
    "Iterator",
    "KeysView",
    "Mapping",
    "MappingView",
    "MutableMapping",
    "MutableSequence",
    "MutableSet",
    "Sequence",
    "Sized",
    "ValuesView",
    "Awaitable",
    "AsyncIterator",
    "AsyncIterable",
    "Coroutine",
    "Collection",
    "AsyncGenerator",
    "AsyncContextManager",
    "Reversible",
    "SupportsAbs",
    "SupportsBytes",
    "SupportsComplex",
    "SupportsFloat",
    "SupportsIndex",
    "SupportsInt",
    "SupportsRound",
    "ChainMap",
    "Counter",
    "Deque",
    "Dict",
    "DefaultDict",
    "List",
    "OrderedDict",
    "Set",
    "FrozenSet",
    "NamedTuple",
    "TypedDict",
    "Generator",
    "BinaryIO",
    "IO",
    "Match",
    "Pattern",
    "TextIO",
    "AnyStr",
    "cast",
    "final",
    "get_args",
    "get_origin",
    "get_type_hints",
    "NewType",
    "no_type_check",
    "no_type_check_decorator",
    "NoReturn",
    "overload",
    "runtime_checkable",
    "Text",
    "TYPE_CHECKING"
  )
}
