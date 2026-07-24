package com.example.server

import com.example.testlib.Fixtures
import com.example.testlib.TestAssertions
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ServerTest {
    private val server = Server()

    @Test
    fun describeNotBlank() {
        TestAssertions.assertNotBlank(server.describe())
    }

    @Test
    fun reverseGreetingHandlesFixture() {
        val result = server.reverseGreeting(Fixtures.sampleName())
        assertTrue(result.contains("ecilA"))
    }

    @Test
    fun squareFactorialOfThree() {
        assertEquals(36L, server.squareFactorial(3))
    }
}
