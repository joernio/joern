package com.example.client

import kotlin.test.Test
import kotlin.test.assertTrue

class ClientTest {
    @Test
    fun runProducesGreeting() {
        val result = Client().run("Bob")
        assertTrue(result.contains("Bob"))
    }
}
