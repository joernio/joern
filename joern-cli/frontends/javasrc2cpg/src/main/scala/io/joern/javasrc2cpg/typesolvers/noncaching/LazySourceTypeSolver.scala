package io.joern.javasrc2cpg.typesolvers.noncaching

import better.files.File
import better.files.File._
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.symbolsolver.model.resolution.SymbolReference
import com.github.javaparser.symbolsolver.model.resolution.TypeSolver
import io.joern.javasrc2cpg.util.SourceParser
import io.joern.javasrc2cpg.util.SourceRootFinder

import java.nio.charset.Charset
import java.nio.file.Files
import java.time.temporal.TemporalAmount
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.jdk.StreamConverters._
import scala.util.Using
import java.nio.file.Path
import java.nio.channels.Pipe.SinkChannel
import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import scala.util.Try
import io.joern.javasrc2cpg.typesolvers.SimpleCombinedTypeSolver
import com.github.javaparser.ast.CompilationUnit
import org.slf4j.LoggerFactory
import com.github.javaparser.symbolsolver.javaparser.Navigator
import scala.quoted.Type
import io.joern.javasrc2cpg.typesolvers.noncaching.LazySourceTypeSolver.SourceFileCandidate
import io.joern.javasrc2cpg.typesolvers.TypeSizeReducer

class LazySourceTypeSolver(
  relativeFilenames: Array[String],
  sourceParser: SourceParser,
  combinedTypeSolver: SimpleCombinedTypeSolver,
  symbolSolver: JavaSymbolSolver
) extends TypeSolver {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Java package declarations are usually found in sources in subdirectories matching the package name, for example
    * the package declaration `package foo.bar.baz` will be defined in `<src_root>/foo/bar/baz/X.class`, so class
    * declarations in these packages can be easily located.
    *
    * This is not enforced by the java compiler, however, so to allow getting type information from files that don't
    * match this standard reprelentation, an index of package declarations in non-standard locations is kept.
    */
  private val packageDirectoryIndex = getPackageDirectoryIndex(relativeFilenames)

  private var parent: TypeSolver = combinedTypeSolver

  override def getParent(): TypeSolver = parent

  override def setParent(parent: TypeSolver): Unit = this.parent = parent

  override def tryToSolveType(className: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    getCorrespondingFile(className)
      .collect { case SourceFileCandidate(packageName, fileName) =>
        sourceParser
          .parseTypesFile(fileName)
          .flatMap(Navigator.findType(_, relativiseClassname(packageName, className)).toScala)
          .flatMap(typeDecl =>
            TypeSizeReducer.simplifyType(typeDecl)
            Try(
              SymbolReference
                .solved(JavaParserFacade.get(combinedTypeSolver).getTypeDeclaration(typeDecl))
            ).toOption
          )
      }
      .flatten
      .getOrElse(SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration]))
  }

  /** To find a nested class a.b.Foo$Bar, the JavaParser Navigator helper class expects the relative class name Foo.Bar
    * when doing the lookup.
    */
  private def relativiseClassname(packageName: String, className: String): String = {
    className.stripPrefix(s"$packageName.")
  }

  private def isMatchingTypeDecl(className: String, typeDecl: TypeDeclaration[_]): Boolean = {
    println(s"${typeDecl.getNameAsString()} -- ${typeDecl.getFullyQualifiedName()}")
    typeDecl.getFullyQualifiedName().toScala match {
      case Some(fullyQualifiedName) => fullyQualifiedName == className

      case None =>
        val classSimpleName = className.split(raw"\.").last
        classSimpleName == typeDecl.getName().getIdentifier()
    }
  }

  /** Try to find the filename for the file containing the class from the class name and the packageDirectoryIndex.
    * Since the class could be a nested class and JavaParser replaces the `$` in class names with `.`, increasingly
    * short package prefixes are considered when attempting to find the class.
    *
    * If multiple matches are found, arbitrarly choose first.
    */
  private def getCorrespondingFile(className: String): Option[SourceFileCandidate] = {
    val nameParts = className.split(raw"\.")
    // For a name a.b.Foo.Bar, the possible top-level class names are (considered in this order):
    //   - a.b.Foo.Bar
    //   - a.b.Foo
    //   - a.b
    //   - a
    val fileCandidates = nameParts.inits.map(namePartsToPossibleFilenames).toArray.flatten

    fileCandidates.find(candidate => sourceParser.doesTypesFileExist(candidate.fileName))
  }

  private def namePartsToPossibleFilenames(classNameParts: Array[String]): Array[SourceFileCandidate] = {
    if (classNameParts.isEmpty) {
      Array.empty
    } else {
      val className   = classNameParts.last
      val packageName = classNameParts.init.mkString(".")

      packageDirectoryIndex
        .get(packageName)
        .collect { possibleDirectories =>
          val filenames = possibleDirectories.map(composeSourceFilePath(_, className))
          SourceFileCandidate.fromResultList(packageName, filenames)
        }
        .getOrElse(Array.empty)
    }
  }

  private def composeSourceFilePath(directory: String, className: String): String = {
    val path = if (directory.isBlank()) {
      Path.of(s"$className.java")
    } else {
      Path.of(directory, s"$className.java")
    }
    path.toString()
  }

  private def getPackageDirectoryIndex(filenames: Array[String]): Map[String, Array[String]] = {
    var index: mutable.Map[String, mutable.ArrayBuffer[String]] = mutable.Map.empty

    filenames.foreach { filename =>
      getPackage(filename) match {
        case None => logger.warn("Could not get package for $filename")

        case Some(packageName) =>
          val directory = Option(Path.of(filename).getParent()).map(_.toString()).getOrElse("")

          index.get(packageName) match {
            case Some(directories) =>
              directories.append(directory)

            case None =>
              index.put(packageName, mutable.ArrayBuffer(directory))
          }
      }
    }

    index.view.mapValues(_.toArray).toMap
  }

  /** Find the declared package by trying to read the package declaration statement. Only read until the first
    * non-whitespace or comment line is found, since the packgae declaration must be the first statement in a Java file.
    *
    * @return
    *   the declared package name or the empty string if it could not be read
    */
  private def getPackage(filename: String): Option[String] = {
    sourceParser
      .getTypesFileLines(filename)
      .toOption
      .map { lines =>
        lines
          .dropWhile(isPrePackageLine)
          .headOption
          .map(_.trim())
          .filter(_.startsWith("package "))
          .map(_.stripPrefix("package ").stripSuffix(";").trim())
          .getOrElse("")
      }
  }
}

/** The package statement must be the first statement in the file, so check that the trimmed string is either empty (an
  * all-whitespace line) or starts with a comment character.
  */
private def isPrePackageLine(line: String): Boolean = {
  // The package statement must be the first statement in the file, so check that the
  // trimmed string is either empty (an all-whitespace line) or starts with a comment
  // character.
  val allowedChars = Set('/', '*')
  line.stripLeading().headOption.filterNot(allowedChars.contains(_)).isEmpty
}

object LazySourceTypeSolver {
  private[LazySourceTypeSolver] case class SourceFileCandidate(packageName: String, fileName: String)
  object SourceFileCandidate {
    private[LazySourceTypeSolver] def fromResultList(
      packageName: String,
      fileCandidates: Array[String]
    ): Array[SourceFileCandidate] = {
      fileCandidates.map(new SourceFileCandidate(packageName, _))
    }
  }
}
