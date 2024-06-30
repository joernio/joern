package io.joern.x2cpg

import ujson.Value

import scala.Option

package object astgen {

  /** The base components of a JSON node.
    */
  trait BaseNodeInfo[T] {
    def node: T
    def json: Value
    def code: String
    def lineNumber: Option[Int]
    def columnNumber: Option[Int]
    def lineNumberEnd: Option[Int]
    def columnNumberEnd: Option[Int]
  }

  /** The basic components of the results from parsing the JSON AST.
    */
  trait BaseParserResult {
    def filename: String
    def fullPath: String
    def json: Value
    def fileContent: String
  }

  /** The default parser result. A minimal implementation of BaseParserResult
    *
    * @param filename
    *   the relative filename
    * @param fullPath
    *   the absolute file path
    * @param json
    *   the deserialized JSON content.
    * @param fileContent
    *   the raw file contents.
    */
  case class ParserResult(filename: String, fullPath: String, json: Value, var fileContent: String)
      extends BaseParserResult

}
