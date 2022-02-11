package io.joern.scanners

object QueryTags {

  val default = "default"

  val posix = "posix"

  val raceCondition = "race-condition"
  val alloc         = "alloc"
  val badfn         = "badfn"
  val badimpl       = "badimpl"
  val setxid        = "setxid"
  val metrics       = "metrics"
  val uaf           = "uaf"
  val xss           = "xss"

  val integers             = "integers"
  val strings              = "strings"
  val sqlInjection         = "sql-injection"
  val compilerOptimization = "compiler-optimization"

}
