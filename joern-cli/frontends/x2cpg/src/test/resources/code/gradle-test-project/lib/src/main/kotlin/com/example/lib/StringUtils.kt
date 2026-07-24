package com.example.lib

object StringUtils {
    fun reverse(input: String): String = input.reversed()

    fun isBlank(input: String?): Boolean = input.isNullOrBlank()
}
