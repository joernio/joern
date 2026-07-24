package com.example.clientcore

import com.example.testlib.Fixtures
import com.example.testlib.TestAssertions
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ClientCoreTest {
    private val clientCore = ClientCore()

    @Test
    fun welcomeUsesFixtures() {
        val result = clientCore.welcome(Fixtures.sampleName())
        TestAssertions.assertNotBlank(result)
        assertTrue(result.contains("Alice"))
    }

    @Test
    fun totalizeSumsThree() {
        assertEquals(6, clientCore.totalize(1, 2, 3))
    }
}
