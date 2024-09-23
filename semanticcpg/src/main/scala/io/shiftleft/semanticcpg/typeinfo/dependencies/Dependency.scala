package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, Version}

import scala.jdk.CollectionConverters._
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}
import java.io.{File, FileReader}
import scala.util.{Failure, Success, Try}

final case class Dependency(name: PackageIdentifier, version: Constraint)
final case class SolvedDependency(name: PackageIdentifier, version: Version)

object Dependency {
//  def fromFile(f: File): Try[List[Dependency]] = {
//    val reader = FileReader(f)
//    val parser: CSVParser = CSVFormat.RFC4180.parse(reader)
//    parser
//      .getRecords()
//      .asScala
//      .map()
//  }
//  
//  private def csvRecordToDependency(rec: CSVRecord): Dependency = {
//    val packageName = rec.get(0)
//    val constraint = rec.get(1)
//    Dependency(PackageIdentifier(packageName), parseConstraint())
//  }
}