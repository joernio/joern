rootProject.name = "gradle-kotlin-test"

include("sharedUtils")
include("core")
include("lib")
include("testLib")
include("clientCore")
include("client")
include("server")
include("emptyModule")
include("mixedSources")
// ghostModule is intentionally NOT included to verify that the dependency fetcher
// handles modules-on-disk that are absent from the Gradle build graph.
