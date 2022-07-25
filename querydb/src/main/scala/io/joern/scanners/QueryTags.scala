package io.joern.scanners

object QueryTags {

  val default = "default"

  val posix   = "posix"
  val android = "android"

  val raceCondition = "race-condition"
  val alloc         = "alloc"
  val badfn         = "badfn"
  val badimpl       = "badimpl"
  val setxid        = "setxid"
  val metrics       = "metrics"
  val uaf           = "uaf"
  val xss           = "xss"

  val integers               = "integers"
  val strings                = "strings"
  val sqlInjection           = "sql-injection"
  val compilerOptimization   = "compiler-optimization"
  val misconfiguration       = "misconfiguration"
  val insecureNetworkTraffic = "insecure-network-traffic"

  val selfDestruct         = "selfdestruct-contract"
  val overflow             = "overflow-contract"
  val controlLoopIteration = "control-loop-iteration-contract"
  val abiEncoder           = "abi-encoder-contract"
  val delegateCall         = "delegateCall-contract"
  val incorrectShift       = "incorrect-shift-contract"
  val modifyingStorageArrayByValue = "modifying-storage-array-by-value-contract"
  val taintedPrivateVariable = "tainted-private-variable-contract"
  val reentrancy = "reentrancy-contract"

}
