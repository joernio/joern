expect fun platformName(): String

fun greet(): String = "${UsedCommon.describe()}: hello from ${platformName()}"
