//> using com.michaelpollmeier:versionsort:1.0.7

val compareResult = versionsort.VersionHelper.compare("1.0", "0.9")
assert(compareResult == 1, s"result of comparison should be `1`, but was `$compareResult`")
