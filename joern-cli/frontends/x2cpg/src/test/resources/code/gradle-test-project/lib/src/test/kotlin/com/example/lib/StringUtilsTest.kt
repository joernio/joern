package com.example.lib

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class StringUtilsTest {
    @Test
    fun reverseReversesString() {
        assertEquals("olleh", StringUtils.reverse("hello"))
    }

    @Test
    fun isBlankDetectsEmpty() {
        assertTrue(StringUtils.isBlank("  "))
        assertFalse(StringUtils.isBlank("x"))
    }
}
