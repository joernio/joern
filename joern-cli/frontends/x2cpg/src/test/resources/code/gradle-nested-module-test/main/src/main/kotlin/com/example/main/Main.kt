package com.example.main

import com.example.core.lib.CoreLib

object Main {
    private val lib = CoreLib()

    @JvmStatic
    fun main(args: Array<String>) {
        val target = args.firstOrNull() ?: "World"
        println(lib.greetLoudly(target))
        println("Sum: ${lib.sum(listOf(1, 2, 3, 4))}")
    }
}
