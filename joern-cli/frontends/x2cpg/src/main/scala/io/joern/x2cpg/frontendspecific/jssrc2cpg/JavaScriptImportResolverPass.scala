package io.joern.x2cpg.frontendspecific.jssrc2cpg

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Method, MethodRef}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.*

import java.io.File as JFile
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}
import scala.util.{Failure, Success, Try}

class JavaScriptImportResolverPass(cpg: Cpg) extends XImportResolverPass(cpg) {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val pathSep     = ":"
    val rawEntity   = importedEntity.stripPrefix("./")
    val alias       = importedAs
    val matcher     = pathPattern.matcher(rawEntity)
    val sep         = Matcher.quoteReplacement(JFile.separator)
    val root        = s"$codeRootDir${JFile.separator}"
    val currentFile = s"$root$fileName"
    // We want to know if the import is local since if an external name is used to match internal methods we may have
    // false paths.
    val isLocalImport = importedEntity.matches("^[.]+/?.*")
    // TODO: At times there is an operation inside of a require, e.g. path.resolve(__dirname + "/../config/env/all.js")
    //  this tries to recover the string but does not perform string constant propagation
    val entity = if (matcher.find()) matcher.group(1) else rawEntity

    val resolvedPath = Try(
      Paths
        .get(currentFile.stripSuffix(currentFile.split(sep).last), entity.split(pathSep).head)
        .toAbsolutePath
        .normalize()
        .toString
        .stripPrefix(root)
    ).getOrElse(entity)

    val isImportingModule = !entity.contains(pathSep)

    def targetModule = Try(
      if (isLocalImport)
        cpg
          .file(s"${Pattern.quote(resolvedPath)}\\.?.*")
          .method
          .nameExact(Defines.Program)
      else
        Iterator.empty
    ) match {
      case Failure(_) =>
        logger.warn(s"Unable to resolve import due to irregular regex at '$importedEntity'")
        Iterator.empty
      case Success(modules) => modules
    }

    def targetAssignments = targetModule
      .nameExact(Defines.Program)
      .flatMap(_._callViaContainsOut)
      .assignment

    val matchingExports = if (isImportingModule) {
      // If we are importing the whole module, we need to load all entities
      targetAssignments
        .code(s"\\_tmp\\_\\d+\\.\\w+ =.*", "(module\\.)?exports.*")
        .dedup
        .l
    } else {
      // If we are importing a specific entity, then we look for it here
      targetAssignments
        .code("^(module.)?exports.*")
        .where(_.argument.codeExact(alias))
        .dedup
        .l
    }

    (if (matchingExports.nonEmpty) {
       matchingExports.flatMap { exp =>
         exp.argument.l match {
           case ::(expCall: Call, ::(b: Identifier, _))
               if expCall.code.matches("^(module.)?exports[.]?.*") && b.name == alias =>
             val moduleMethods      = targetModule.repeat(_.astChildren.isMethod)(_.emit).l
             lazy val methodMatches = moduleMethods.name(b.name).l
             lazy val constructorMatches =
               moduleMethods.fullName(s".*${b.name}$pathSep${XDefines.ConstructorMethodName}$$").l
             lazy val moduleExportsThisVariable = moduleMethods.body.local
               .where(_.nameExact(b.name))
               .nonEmpty
             // Exported function with only the name of the function
             val methodPaths =
               if (methodMatches.nonEmpty) methodMatches.fullName.toSet
               else constructorMatches.fullName.toSet
             if (methodPaths.nonEmpty) {
               methodPaths.flatMap(x => Set(ResolvedMethod(x, alias, Option("this")), ResolvedTypeDecl(x)))
             } else if (moduleExportsThisVariable) {
               Set(ResolvedMember(targetModule.fullName.head, b.name))
             } else {
               Set.empty
             }
           case ::(x: Call, ::(b: MethodRef, _)) =>
             // Exported function with a method ref of the function
             val methodName = x.argumentOption(2).map(_.code).getOrElse(b.referencedMethod.name)
             val (callName, receiver) =
               if (methodName == "exports") (alias, Option("this")) else (methodName, Option(alias))
             b.referencedMethod.astParent.iterator
               .collectAll[Method]
               .fullName
               .map(x => ResolvedTypeDecl(x))
               .toSet ++ Set(ResolvedMethod(b.methodFullName, callName, receiver))
           case ::(_, ::(y: Call, _)) =>
             // Exported closure with a method ref within the AST of the RHS
             y.ast.isMethodRef.map(mRef => ResolvedMethod(mRef.methodFullName, alias, Option("this"))).toSet
           case _ =>
             Set.empty[EvaluatedImport]
         }
       }.toSet
     } else {
       Set(UnknownMethod(entity, alias, Option("this")), UnknownTypeDecl(entity))
     }).foreach(x => evaluatedImportToTag(x, importCall, diffGraph))
  }

}
