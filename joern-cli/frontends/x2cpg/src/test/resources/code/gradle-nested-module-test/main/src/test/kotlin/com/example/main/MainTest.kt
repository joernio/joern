package com.example.main

import com.example.core.lib.CoreLib
import kotlin.test.Test
import kotlin.test.assertEquals

class MainTest {
    @Test
    fun libIsReachableFromMain() {
        val lib = CoreLib()
        assertEquals("HELLO, WORLD!", lib.greetLoudly("World"))
    }
}
