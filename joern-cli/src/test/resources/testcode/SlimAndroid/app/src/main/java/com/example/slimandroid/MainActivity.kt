package com.example.slimandroid

import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity
import com.bumptech.glide.Glide

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val imageUrl = "http://phrack.org/images/phrack-logo.jpg"
        Glide.with(applicationContext).load(imageUrl)
    }
}