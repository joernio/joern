package com.example.core.lib

import kotlin.test.Test
import kotlin.test.assertEquals

class CoreLibTest {
    private val lib = CoreLib()

    @Test
    fun greetLoudlyUppercases() {
        assertEquals("HELLO, WORLD!", lib.greetLoudly("World"))
    }

    @Test
    fun sumOfValues() {
        assertEquals(6, lib.sum(listOf(1, 2, 3)))
    }
}
