package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignatedInitializer
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTConstructorInitializer
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDesignatedInitializer
import org.eclipse.cdt.internal.core.dom.parser.c.CASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTArrayRangeDesignator

trait AstForInitializersCreator { this: AstCreator =>

  protected def astForInitializerList(l: IASTInitializerList): Ast = {
    val MAX_INITIALIZERS = 1000
    val op               = Operators.arrayInitializer
    val initCallNode     = callNode(l, code(l), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val clauses          = l.getClauses.slice(0, MAX_INITIALIZERS)
    val args             = clauses.toList.map(x => astForNode(x))
    val ast              = callAst(initCallNode, args)
    if (l.getClauses.length > MAX_INITIALIZERS) {
      val placeholder =
        literalNode(l, "<too-many-initializers>", Defines.Any).argumentIndex(MAX_INITIALIZERS)
      ast.withChild(Ast(placeholder)).withArgEdge(initCallNode, placeholder)
    } else {
      ast
    }
  }

  protected def astForCPPASTConstructorInitializer(c: ICPPASTConstructorInitializer): Ast = {
    val name      = Defines.OperatorConstructorInitializer
    val callNode_ = callNode(c, code(c), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val args      = c.getArguments.toList.map(a => astForNode(a))
    callAst(callNode_, args)
  }

  protected def astForCASTDesignatedInitializer(d: ICASTDesignatedInitializer): Ast = {
    val node = blockNode(d)
    scope.pushNewBlockScope(node)
    val op = Operators.assignment
    val calls = withIndex(d.getDesignators) { (des, o) =>
      val callNode_ =
        callNode(d, code(d), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
          .argumentIndex(o)
      val left  = astForNode(des)
      val right = astForNode(d.getOperand)
      callAst(callNode_, List(left, right))
    }
    scope.popScope()
    blockAst(node, calls.toList)
  }

  protected def astForCPPASTDesignatedInitializer(d: ICPPASTDesignatedInitializer): Ast = {
    val node = blockNode(d)
    scope.pushNewBlockScope(node)
    val op = Operators.assignment
    val calls = withIndex(d.getDesignators) { (des, o) =>
      val callNode_ =
        callNode(d, code(d), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
          .argumentIndex(o)
      val left  = astForNode(des)
      val right = astForNode(d.getOperand)
      callAst(callNode_, List(left, right))
    }
    scope.popScope()
    blockAst(node, calls.toList)
  }

  protected def astForCASTArrayRangeDesignator(des: CASTArrayRangeDesignator): Ast = {
    val op         = Operators.arrayInitializer
    val callNode_  = callNode(des, code(des), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val floorAst   = nullSafeAst(des.getRangeFloor)
    val ceilingAst = nullSafeAst(des.getRangeCeiling)
    callAst(callNode_, List(floorAst, ceilingAst))
  }

  protected def astForCPPASTArrayRangeDesignator(des: CPPASTArrayRangeDesignator): Ast = {
    val op         = Operators.arrayInitializer
    val callNode_  = callNode(des, code(des), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val floorAst   = nullSafeAst(des.getRangeFloor)
    val ceilingAst = nullSafeAst(des.getRangeCeiling)
    callAst(callNode_, List(floorAst, ceilingAst))
  }

}
