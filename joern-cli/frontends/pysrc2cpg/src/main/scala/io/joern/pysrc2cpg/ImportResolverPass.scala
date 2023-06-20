package io.joern.pysrc2cpg

import better.files.{File => BFile}
import io.joern.x2cpg.passes.frontend.ImportsPass._
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

import java.io.{File => JFile}
import java.util.regex.{Matcher, Pattern}

class ImportResolverPass(cpg: Cpg) extends XImportResolverPass(cpg) {

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val (namespace, entityName) = if (importedEntity.contains(".")) {
      val splitName = importedEntity.split('.').toSeq
      val namespace = importedEntity.stripSuffix(s".${splitName.last}")
      (relativizeNamespace(namespace, fileName), splitName.last)
    } else {
      ("", importedEntity)
    }

    resolveEntities(namespace, entityName, importedAs).foreach(x => resolvedImportToTag(x, importCall, diffGraph))
  }

  private def relativizeNamespace(path: String, fileName: String): String = if (path.startsWith(".")) {
    // TODO: pysrc2cpg does not link files to the correct namespace nodes
    val sep = Matcher.quoteReplacement(JFile.separator)
    // The below gives us the full path of the relative "."
    val relativeNamespace =
      if (fileName.contains(JFile.separator))
        fileName.substring(0, fileName.lastIndexOf(JFile.separator)).replaceAll(sep, ".")
      else ""
    (if (path.length > 1) relativeNamespace + path.replaceAll(sep, ".")
     else relativeNamespace).stripPrefix(".")
  } else path

  /** For an import - given by its module path and the name of the imported function or module - determine the possible
    * callee names.
    *
    * @param path
    *   the module path.
    * @param expEntity
    *   the name of the imported entity. This could be a function, module, or variable/field.
    * @param alias
    *   how the imported entity is named.
    * @return
    *   the possible callee names
    */
  private def resolveEntities(path: String, expEntity: String, alias: String): Set[ResolvedImport] = {

    implicit class ResolvedNodeExt(val traversal: Seq[String]) {
      def toResolvedImport(cpg: Cpg): Seq[ResolvedImport] = {
        val resolvedEntities =
          traversal.flatMap(x => cpg.typeDecl.fullNameExact(x) ++ cpg.method.fullNameExact(x)).collect {
            case x: Method   => ResolvedMethod(x.fullName, alias, Some("self"))
            case x: TypeDecl => ResolvedTypeDecl(x.fullName)
          }
        if (resolvedEntities.isEmpty) {
          traversal.filterNot(_.contains("__init__.py")).map(x => UnknownImport(x))
        } else {
          resolvedEntities
        }
      }
    }

    implicit class CalleeAsInitExt(val name: String) {
      def asInit: String = if (name.contains("__init__.py")) name
      else name.replace(".py", s"${JFile.separator}__init__.py")

      def withInit: Seq[String] = Seq(name, name.asInit)
    }

    val pathSep            = "."
    val sep                = Matcher.quoteReplacement(JFile.separator)
    val isMaybeConstructor = expEntity.split("\\.").lastOption.exists(s => s.nonEmpty && s.charAt(0).isUpper)

    lazy val membersMatchingImports: List[(TypeDecl, Member)] = cpg.typeDecl
      .fullName(s".*${Pattern.quote(path)}.*")
      .flatMap(t =>
        t.member.nameExact(expEntity).headOption match {
          case Some(member) => Option((t, member))
          case None         => None
        }
      )
      .toList

    (path match {
      case "" if expEntity.contains(".") =>
        // Case 1: Qualified path: import foo.bar => (bar.py or bar/__init__.py)
        val splitFunc = expEntity.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        s"${splitFunc(0)}.py:<module>$pathSep$name".withInit.toResolvedImport(cpg)
      case "" =>
        // Case 2: import of a module: import foo => (foo.py or foo/__init__.py)
        s"$expEntity.py:<module>".withInit.toResolvedImport(cpg)
      case _ if membersMatchingImports.nonEmpty =>
        // Case 3: import of a variable: from api import db => (api.py or foo.__init__.py) @ identifier(db)
        membersMatchingImports.map {
          case (t, m) if t.method.nameExact(m.name).nonEmpty =>
            ResolvedMethod(t.method.nameExact(m.name).fullName.head, alias)
          case (t, m) if t.astSiblings.isMethod.fullNameExact(t.fullName).ast.isTypeDecl.nameExact(m.name).nonEmpty =>
            ResolvedTypeDecl(
              t.astSiblings.isMethod.fullNameExact(t.fullName).ast.isTypeDecl.nameExact(m.name).fullName.head
            )
          case (t, m) => ResolvedMember(t.fullName, m.name)
        }
      case _ =>
        // Case 4:  Import from module using alias, e.g. import bar from foo as faz
        val fileOrDir = BFile(codeRoot) / path
        val pyFile    = BFile(codeRoot) / s"$path.py"
        fileOrDir match {
          case f if f.isDirectory && !pyFile.exists =>
            Seq(s"${path.replaceAll("\\.", sep)}${java.io.File.separator}$expEntity.py:<module>").toResolvedImport(cpg)
          case f if f.isDirectory && (f / s"$expEntity.py").exists =>
            Seq(s"${(f / s"$expEntity.py").pathAsString.stripPrefix(codeRoot)}:<module>").toResolvedImport(cpg)
          case _ =>
            s"${path.replaceAll("\\.", sep)}.py:<module>$pathSep$expEntity".withInit.toResolvedImport(cpg)
        }
    }).flatMap {
      // If we import the constructor, we also import the type
      case x: ResolvedMethod if isMaybeConstructor =>
        Seq(ResolvedMethod(Seq(x.fullName, "__init__").mkString(pathSep), alias), ResolvedTypeDecl(x.fullName))
      // If we import the type, we also import the constructor
      case x: ResolvedTypeDecl if !x.fullName.endsWith("<module>") =>
        Seq(x, ResolvedMethod(Seq(x.fullName, "__init__").mkString(pathSep), alias))
      // If we can determine the import is a constructor, then it is likely not a member
      case x: UnknownImport if isMaybeConstructor =>
        Seq(UnknownMethod(Seq(x.path, "__init__").mkString(pathSep), alias), UnknownTypeDecl(x.path))
      case x => Seq(x)
    }.toSet
  }
}
