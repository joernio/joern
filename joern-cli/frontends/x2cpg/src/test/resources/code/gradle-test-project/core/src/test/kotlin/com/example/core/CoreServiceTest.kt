package com.example.core

import kotlin.test.Test
import kotlin.test.assertEquals

class CoreServiceTest {
    private val service = CoreService()

    @Test
    fun greetReturnsExpected() {
        assertEquals("Hello, World!", service.greet("World"))
    }

    @Test
    fun addSums() {
        assertEquals(5, service.add(2, 3))
    }
}
