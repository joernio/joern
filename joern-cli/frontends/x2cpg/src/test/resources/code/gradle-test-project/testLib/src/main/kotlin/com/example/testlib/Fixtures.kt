package com.example.testlib

import com.example.lib.StringUtils

object Fixtures {
    fun sampleName(): String = "Alice"

    fun sampleReversedName(): String = StringUtils.reverse(sampleName())
}
