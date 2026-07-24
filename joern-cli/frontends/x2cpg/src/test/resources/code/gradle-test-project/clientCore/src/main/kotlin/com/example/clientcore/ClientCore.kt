package com.example.clientcore

import com.example.core.CoreService

class ClientCore {
    private val coreService = CoreService()

    fun welcome(user: String): String =
        "${coreService.greet(user)} Welcome to the client."

    fun totalize(a: Int, b: Int, c: Int): Int =
        coreService.add(coreService.add(a, b), c)
}
