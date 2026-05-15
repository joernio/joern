package io.joern.x2cpg.frontendspecific.pysrc2cpg

object PythonTypeStubs {

  private val b = Constants.builtinPrefix

  private val stubReturnTypes: Map[String, String] = Map(
    s"${b}len" -> s"${b}int", s"${b}int" -> s"${b}int", s"${b}str" -> s"${b}str",
    s"${b}float" -> s"${b}float", s"${b}bool" -> s"${b}bool", s"${b}bytes" -> s"${b}bytes",
    s"${b}list" -> s"${b}list", s"${b}dict" -> s"${b}dict", s"${b}set" -> s"${b}set",
    s"${b}tuple" -> s"${b}tuple", s"${b}type" -> s"${b}type", s"${b}repr" -> s"${b}str",
    s"${b}abs" -> s"${b}int", s"${b}hash" -> s"${b}int", s"${b}id" -> s"${b}int",
    s"${b}ord" -> s"${b}int", s"${b}chr" -> s"${b}str", s"${b}hex" -> s"${b}str",
    s"${b}oct" -> s"${b}str", s"${b}bin" -> s"${b}str", s"${b}range" -> s"${b}range",
    s"${b}enumerate" -> s"${b}enumerate", s"${b}zip" -> s"${b}zip",
    s"${b}map" -> s"${b}map", s"${b}filter" -> s"${b}filter",
    s"${b}sorted" -> s"${b}list", s"${b}reversed" -> s"${b}reversed",
    s"${b}isinstance" -> s"${b}bool", s"${b}issubclass" -> s"${b}bool",
    s"${b}hasattr" -> s"${b}bool", s"${b}callable" -> s"${b}bool",
    s"${b}sum" -> s"${b}int", s"${b}round" -> s"${b}int",
    s"${b}input" -> s"${b}str", s"${b}open" -> "io.TextIOWrapper",
    s"${b}format" -> s"${b}str",
    "len" -> s"${b}int", "int" -> s"${b}int", "str" -> s"${b}str",
    "float" -> s"${b}float", "bool" -> s"${b}bool", "bytes" -> s"${b}bytes",
    "list" -> s"${b}list", "dict" -> s"${b}dict", "set" -> s"${b}set",
    "tuple" -> s"${b}tuple", "type" -> s"${b}type", "repr" -> s"${b}str",
    "abs" -> s"${b}int", "hash" -> s"${b}int", "id" -> s"${b}int",
    "ord" -> s"${b}int", "chr" -> s"${b}str", "hex" -> s"${b}str",
    "oct" -> s"${b}str", "bin" -> s"${b}str", "range" -> s"${b}range",
    "enumerate" -> s"${b}enumerate", "zip" -> s"${b}zip",
    "map" -> s"${b}map", "filter" -> s"${b}filter",
    "sorted" -> s"${b}list", "reversed" -> s"${b}reversed",
    "isinstance" -> s"${b}bool", "issubclass" -> s"${b}bool",
    "hasattr" -> s"${b}bool", "callable" -> s"${b}bool",
    "sum" -> s"${b}int", "round" -> s"${b}int",
    "input" -> s"${b}str", "open" -> "io.TextIOWrapper", "format" -> s"${b}str",
    "os.path.join" -> s"${b}str", "os.path.exists" -> s"${b}bool",
    "os.path.isfile" -> s"${b}bool", "os.path.isdir" -> s"${b}bool",
    "os.path.basename" -> s"${b}str", "os.path.dirname" -> s"${b}str",
    "os.getcwd" -> s"${b}str", "os.listdir" -> s"${b}list",
    "json.loads" -> s"${b}dict", "json.dumps" -> s"${b}str",
    "json.load" -> s"${b}dict"
  )

  def returnTypeFor(name: String): Option[String] = stubReturnTypes.get(name)
}
