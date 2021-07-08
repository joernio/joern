name := "ghidra2cpg"

enablePlugins(JavaAppPackaging)
publish / skip := true

libraryDependencies += "io.joern" %% "ghidra2cpg" % Versions.ghidra2cpg

Compile/mainClass := Some("io.joern.ghidra2cpg.Main")

maintainer := "fabs@shiftleft.io"
