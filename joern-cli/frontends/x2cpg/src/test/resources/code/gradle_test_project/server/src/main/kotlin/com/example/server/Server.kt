package com.example.server

import com.example.core.CoreConstants
import com.example.core.CoreService
import com.example.lib.MathUtils
import com.example.lib.StringUtils

class Server {
    private val coreService = CoreService()

    fun describe(): String = "Server v${CoreConstants.VERSION}"

    fun reverseGreeting(name: String?): String {
        if (StringUtils.isBlank(name)) {
            return coreService.greet("guest")
        }
        return StringUtils.reverse(coreService.greet(name!!))
    }

    fun squareFactorial(n: Int): Long =
        MathUtils.square(MathUtils.factorial(n).toInt()).toLong()
}
