package com.example.testlib

import kotlin.test.Test
import kotlin.test.assertEquals

class FixturesTest {
    @Test
    fun sampleNameIsConsistent() {
        assertEquals("Alice", Fixtures.sampleName())
    }

    @Test
    fun sampleReversedNameIsCorrect() {
        assertEquals("ecilA", Fixtures.sampleReversedName())
    }
}
