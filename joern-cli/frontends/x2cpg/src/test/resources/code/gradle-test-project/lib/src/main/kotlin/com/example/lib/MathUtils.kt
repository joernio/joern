package com.example.lib

object MathUtils {
    fun square(x: Int): Int = x * x

    fun factorial(n: Int): Long {
        require(n >= 0) { "n must be non-negative" }
        var result = 1L
        for (i in 2..n) result *= i
        return result
    }
}
