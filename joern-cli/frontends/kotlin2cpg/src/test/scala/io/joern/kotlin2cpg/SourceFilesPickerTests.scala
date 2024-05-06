package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.files.SourceFilesPicker

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll

class SourceFilesPickerTests extends AnyFreeSpec with Matchers with BeforeAndAfterAll {

  "SourceFilesPicker" - {
    "should not filter out fileNames ending in `build.gradle`" in {
      val inFileName = "/path/does/not/exist/build.gradle"
      SourceFilesPicker.shouldFilter(inFileName) shouldBe false
    }
  }

}
