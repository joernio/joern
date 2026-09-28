package io.joern.console

/** JSON serialization for queries, based on ujson/upickle.
  *
  * Fields are written in declaration order and the `traversal` function, which cannot be serialized, is rendered as an
  * empty object.
  */
object QueryJsonSerialization {

  private implicit val codeSnippetRw: upickle.default.ReadWriter[CodeSnippet]         = upickle.default.macroRW
  private implicit val codeExamplesRw: upickle.default.ReadWriter[CodeExamples]       = upickle.default.macroRW
  private implicit val multiFileRw: upickle.default.ReadWriter[MultiFileCodeExamples] = upickle.default.macroRW

  def write(queries: List[Query]): String = ujson.write(ujson.Arr(queries.map(toJson)*))

  private def toJson(query: Query): ujson.Obj = {
    ujson.Obj(
      "name"                  -> query.name,
      "author"                -> query.author,
      "title"                 -> query.title,
      "description"           -> query.description,
      "score"                 -> query.score,
      "traversal"             -> ujson.Obj(), // functions cannot be serialized - rendered as an empty object
      "traversalAsString"     -> query.traversalAsString,
      "tags"                  -> query.tags,
      "language"              -> query.language,
      "codeExamples"          -> upickle.default.writeJs(query.codeExamples),
      "multiFileCodeExamples" -> upickle.default.writeJs(query.multiFileCodeExamples)
    )
  }

}
