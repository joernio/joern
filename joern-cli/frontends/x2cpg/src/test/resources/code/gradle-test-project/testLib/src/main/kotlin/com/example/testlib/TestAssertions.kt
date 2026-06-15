package com.example.testlib

import com.example.lib.StringUtils

object TestAssertions {
    fun assertNotBlank(value: String?) {
        if (StringUtils.isBlank(value)) {
            throw AssertionError("Expected non-blank value but got: '$value'")
        }
    }
}
