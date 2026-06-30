package com.example

import android.app.Activity
import android.os.Bundle
import com.example.used.UsedKotlin

class MainActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        println(JavaSource.hello())
        println(UsedKotlin.describe())
    }
}
