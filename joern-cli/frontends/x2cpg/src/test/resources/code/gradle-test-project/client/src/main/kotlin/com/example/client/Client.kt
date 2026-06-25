package com.example.client

import com.example.clientcore.*
import java.util.*

class Client {
    private val clientCore = ClientCore()

    fun run(user: String): String = clientCore.welcome(user)
}

fun main(args: Array<String>) {
    val name = args.firstOrNull() ?: "World"
    println(Client().run(name))
}
