package io.joern.x2cpg.datastructures

import io.joern.x2cpg.datastructures.DefaultImportResolver.ResolvablePackage
import io.joern.x2cpg.datastructures.ImportResolver.{Import, Resolvable}
import io.joern.x2cpg.datastructures.ModuleResolver.Module
import io.shiftleft.codepropertygraph.generated.nodes.{Local, Member, Method, Namespace, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, NodeTypes, PropertyNames}
import scala.collection.mutable

/** Describes a class that resolves imports from a given file system and code layout.
  */
trait ImportResolver {

  import ImportResolver.*

  /** @return
    *   the file system model.
    */
  protected lazy val fileSystem: FileSystem

  /** Resolves an entity against an import.
    * @param i
    *   an import containing entity details.
    * @return
    *   if unsuccessful, some entity stub, if successful, a collection of resolvable entities.
    */
  def resolve(i: Import): Either[Entity, Seq[Entity & Resolvable[?]]]

}

object ImportResolver {

  /** An import statement referring to some external but potentially resolvable entity.
    */
  case class Import(name: String, entities: Seq[Entity], alias: Option[String] = None, importingFile: String)

  /** Some exposed code entity that may be importable.
    */
  trait Entity {

    /** @return
      *   the entity identifier.
      */
    def name: String

    /** @return
      *   the file containing this entity.
      */
    def filename: String
  }

  /** An entity that may be resolved back to a node in the implementing analysis.
    * @tparam E
    *   the resolved object from the implementing analysis.
    */
  trait Resolvable[E] {

    /** @return
      *   the callback function that will resolve the entity of the import resolver to an object in the implementing
      *   analysis.
      */
    def resolver: () => E

  }

  /** A package or namespace containing importable entities.
    */
  trait PackageLike extends Entity {

    /** The delimiter between package names in the fully qualified name.
      */
    protected val packageDelimiter: String = "."

    /** @return
      *   the fully qualified name of this package-like.
      */
    lazy val fullName: String = {
      def recurseName(x: PackageLike): String = x.parent match {
        case Some(parentPackage) => s"${recurseName(parentPackage)}$packageDelimiter${x.name}"
        case None                => x.name
      }
      recurseName(this)
    }

    /** @return
      *   the entities which may be resolved to some node in the analysis.
      */
    def entities: Seq[Entity]

    /** @return
      *   the parent package, if not the root package.
      */
    def parent: Option[PackageLike]

    /** @return
      *   children packages, if any.
      */
    def children: Seq[PackageLike]

  }

  /** A file system to model entities in an import analysis.
    *
    * @param rootPaths
    *   the root paths to concat import paths against. Languages such as Ruby may specify multiple with the $LOAD_PATH
    *   variable.
    * @param fileToEntityMap
    *   a mapping between relative files and entities.
    */
  class FileSystem private (rootPaths: Seq[String], fileToEntityMap: Map[String, Vector[Entity]]) {

    /** @param filename
      *   the relative file name.
      * @return
      *   the entities associated with this file.
      */
    def entitiesFor(filename: String): Vector[Entity] = {
      fileToEntityMap.getOrElse(filename, Vector.empty)
    }

  }

  object FileSystem {

    /** A file system to model entities in an import analysis.
      *
      * @param rootPaths
      *   the root paths to concat import paths against. Languages such as Ruby may specify multiple with the $LOAD_PATH
      *   variable.
      */
    class FileSystemBuilder(rootPaths: Seq[String]) {

      private val packages        = mutable.Seq.empty[PackageLike]
      private val fileToEntityMap = mutable.Map.empty[String, mutable.Set[Entity]]

      def addEntity(e: Entity): this.type = {
        fileToEntityMap.getOrElseUpdate(e.filename, mutable.Set.empty).add(e)
        this
      }

      def addEntities(es: Seq[Entity]): this.type = {
        es.groupBy(_.filename).foreach { case (filename, ees) =>
          fileToEntityMap.getOrElseUpdate(filename, mutable.Set.empty).addAll(ees)
        }
        this
      }

      def build: FileSystem = new FileSystem(rootPaths, fileToEntityMap.map { case (k, v) => k -> v.toVector }.toMap)

    }
  }

}

/** For systems where a module-like behaviour is found in addition to namespaces/packages.
  */
object ModuleResolver {

  import ImportResolver.*

  /** A module such as those found in Python/JavaScript.
    * @param name
    *   the module name.
    * @param filename
    *   the containing file.
    * @param entities
    *   the entities this module exposes.
    * @param parent
    *   the parent package-like
    * @param children
    *   children package-likes, which in this context are other modules.
    */
  class Module(
    val name: String,
    val filename: String,
    val entities: Seq[Entity],
    val parent: Option[PackageLike] = None,
    val children: Seq[PackageLike] = Seq.empty
  ) extends PackageLike

}

/** Provides a set of useful CPG-based entities.
  */
object DefaultImportResolver {

  import ImportResolver.*
  import ModuleResolver.*

  /** A basic package.
    *
    * @param name
    *   the package name.
    * @param filename
    *   the directory or containing file.
    * @param entities
    *   the entities which can be imported from this package.
    * @param parent
    *   the parent package.
    * @param children
    *   the children packages.
    */
  class Package(
    override val name: String,
    override val filename: String,
    override val entities: Seq[Entity],
    override val parent: Option[PackageLike] = None,
    override val children: Seq[PackageLike] = Seq.empty
  ) extends PackageLike

  /** A basic resolvable package.
    * @param resolver
    *   the callback for the respective CPG namespace node.
    */
  case class ResolvablePackage(
    override val name: String,
    override val filename: String,
    override val entities: Seq[Entity],
    override val parent: Option[PackageLike] = None,
    override val children: Seq[PackageLike] = Seq.empty,
    resolver: () => Namespace
  ) extends Package(name, filename, entities, parent, children)
      with Resolvable[Namespace]

  /** A basic resolvable function.
    * @param name
    *   the method name.
    * @param resolver
    *   a callback for the respective CPG method node.
    */
  case class ResolvableFunction(name: String, filename: String, resolver: () => Method)
      extends Entity
      with Resolvable[Method]

  /** A basic resolvable type declaration.
    *
    * @param name
    *   the type declaration name.
    * @param resolver
    *   a callback for the respective CPG type decl node.
    */
  case class ResolvableTypeDecl(name: String, filename: String, resolver: () => TypeDecl)
      extends Entity
      with Resolvable[TypeDecl]

  /** A basic resolvable member.
    *
    * @param name
    *   the member name.
    * @param resolver
    *   a callback for the respective CPG member node.
    */
  case class ResolvableMember(name: String, filename: String, resolver: () => Member)
      extends Entity
      with Resolvable[Member]

  /** A resolvable module.
    *
    * @param resolver
    *   the callback for the respective CPG method node.
    */
  case class ResolvableModule(
    override val name: String,
    override val filename: String,
    override val entities: Seq[Entity],
    override val parent: Option[PackageLike] = None,
    override val children: Seq[PackageLike] = Seq.empty,
    resolver: () => Method
  ) extends Module(name, filename, entities, parent, children)
      with Resolvable[Method]

  /** A basic resolvable variable. Modules may export module-level variables
    *
    * @param name
    *   the local variable name.
    * @param resolver
    *   a callback for the respective CPG local node.
    */
  case class ResolvableVariable(name: String, filename: String, resolver: () => Local)
      extends Entity
      with Resolvable[Local]

}
