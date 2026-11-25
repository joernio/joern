package io.joern.javasrc2cpg.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DelombokSourceMapTests extends AnyWordSpec with Matchers {

  private val issue5330Original =
    """package com.test.dao.db.impl;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Map;
import org.springframework.stereotype.Repository;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class QuartzDAOImplDefault implements QuartzDAO {
    private ScanData deserializeJobData(byte[] jobData) {
            ByteArrayInputStream bis = new ByteArrayInputStream(jobData);
            ObjectInputStream ois = new ObjectInputStream(bis);
            Map<String, Object> jobDataMap = (Map<String, Object>) ois.readObject();
    }
}"""

  private val issue5330Delomboked =
    """package com.test.dao.db.impl;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Map;
import java.util.logging.Logger;
import org.springframework.stereotype.Repository;

public class QuartzDAOImplDefault implements QuartzDAO {
    private static final Logger log = Logger.getLogger(QuartzDAOImplDefault.class.getName());
    private ScanData deserializeJobData(byte[] jobData) {
            ByteArrayInputStream bis = new ByteArrayInputStream(jobData);
            ObjectInputStream ois = new ObjectInputStream(bis);
            Map<String, Object> jobDataMap = (Map<String, Object>) ois.readObject();
    }
}"""

  "DelombokSourceMap.build" should {
    "return identity for equal inputs" in {
      DelombokSourceMap.build("a\nb", "a\nb").get shouldBe DelombokSourceMap.Identity
    }

    "map readObject from delomboked text back to original line 17" in {
      val sourceMap              = DelombokSourceMap.build(issue5330Original, issue5330Delomboked).get
      val delomLines             = issue5330Delomboked.split("\n")
      val readObjectDelomLine    = delomLines.indexWhere(_.contains("readObject")) + 1
      val originalReadObjectLine = issue5330Original.split("\n").indexWhere(_.contains("readObject")) + 1
      sourceMap.translateLine(readObjectDelomLine) shouldBe originalReadObjectLine
      val delomReadObjectColumn    = issue5330Delomboked.split("\n")(readObjectDelomLine - 1).indexOf("ois") + 1
      val originalReadObjectColumn = issue5330Original.split("\n")(originalReadObjectLine - 1).indexOf("ois") + 1
      sourceMap.translateColumn(readObjectDelomLine, delomReadObjectColumn) shouldBe originalReadObjectColumn
    }

    "collapse generated logger field onto the @Slf4j line" in {
      val sourceMap         = DelombokSourceMap.build(issue5330Original, issue5330Delomboked).get
      val slf4jLine         = issue5330Original.split("\n").indexWhere(_.contains("@Slf4j")) + 1
      val logFieldDelomLine =
        issue5330Delomboked.split("\n").indexWhere(_.contains("private static final Logger log")) + 1
      sourceMap.translateLine(logFieldDelomLine) shouldBe slf4jLine
    }

    "map unrelated single lines via generated projection" in {
      val original   = "only_in_original"
      val delomboked = "only_in_delomboked"
      val sourceMap  = DelombokSourceMap.build(original, delomboked).get
      sourceMap.translateLine(1) shouldBe 1
    }
  }
}
