package com.example.core.lib

import com.example.core.CoreService

class CoreLib {
    private val coreService = CoreService()

    fun greetLoudly(name: String): String = coreService.greet(name).uppercase()

    fun sum(values: List<Int>): Int = values.fold(0) { acc, v -> coreService.add(acc, v) }
}
