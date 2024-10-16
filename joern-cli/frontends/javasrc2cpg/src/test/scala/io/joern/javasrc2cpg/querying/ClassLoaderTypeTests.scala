package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.utils.ExternalCommand

class ClassLoaderTypeTests extends JavaSrcCode2CpgFixture {
  private lazy val jdk17Home = sys.env.get("JAVA_17_HOME").get
  private lazy val jdk11Home = sys.env.get("JAVA_11_HOME").get
  private lazy val jdk8Home  = sys.env.get("JAVA_8_HOME").get

  "types with java 17 exclusive features" ignore {
    val testCode =
      """
     |import javax.swing.filechooser.FileSystemView;
     |import java.io.File;
     |
     |public class Test {
     |    public static void main(String[] args) {
     |      FileSystemView fsv = FileSystemView.getFileSystemView();
     |      fsv.getSystemIcon(new File("application.exe"), 64, 64).getIconHeight();
     |    }
     |}
     |""".stripMargin

    "be resolved with a java 17 classloader" in {
      val config = Config().withJdkPath(jdk17Home)
      val cpg    = code(testCode).withConfig(config)

      val expected = "javax.swing.filechooser.FileSystemView.getSystemIcon:javax.swing.Icon(java.io.File,int,int)"
      cpg.call.name("getSystemIcon").methodFullName.head shouldEqual expected
      cpg.call.name("getIconHeight").methodFullName.head shouldBe "javax.swing.Icon.getIconHeight:int()"
    }

    "not be resolved with a java 11 classloader" in {
      val config = Config().withJdkPath(jdk11Home)
      val cpg    = code(testCode).withConfig(config)

      cpg.call.name("getIconHeight").methodFullName.head.startsWith("<unresolved") shouldBe true
    }

    "not be resolved with a java 8 classloader" in {
      val config = Config().withJdkPath(jdk8Home)
      val cpg    = code(testCode).withConfig(config)

      cpg.call.name("getIconHeight").methodFullName.head.startsWith("<unresolved") shouldBe true
    }

    "be resolved by the system classloader (java 17)" in {
      val cpg = code(testCode)

      cpg.call.name("getIconHeight").methodFullName.head.startsWith("<unresolved") shouldBe false
    }
  }

  "types with java > 8 features" ignore {
    val testCode =
      """
     |public class Test {
     |    public static void foo(String s) {
     |        s.strip().isBlank();
     |    }
     |}
     |""".stripMargin

    "be resolved by the java 17 classloader" in {
      val config = Config().withJdkPath(jdk17Home)
      val cpg    = code(testCode).withConfig(config)

      cpg.call.name("strip").methodFullName.head shouldEqual "java.lang.String.strip:java.lang.String()"
      cpg.call.name("isBlank").methodFullName.head shouldEqual "java.lang.String.isBlank:boolean()"
    }

    "be resolved by the java 11 classloader" in {
      val config = Config().withJdkPath(jdk11Home)
      val cpg    = code(testCode).withConfig(config)

      cpg.call.name("strip").methodFullName.head shouldEqual "java.lang.String.strip:java.lang.String()"
      cpg.call.name("isBlank").methodFullName.head shouldEqual "java.lang.String.isBlank:boolean()"
    }

    "not be resolved by the java 8 classloader" in {
      val config = Config().withJdkPath(jdk8Home)
      val cpg    = code(testCode).withConfig(config)

      cpg.call.name("strip").methodFullName.head shouldBe (
        "java.lang.String.strip:<unresolvedSignature>(0)"
      )
      cpg.call.name("isBlank").methodFullName.head shouldBe (
        "<unresolvedNamespace>.isBlank:<unresolvedSignature>(0)"
      )
    }
  }

  "types with java 8 features should be resolved with a jdk path override" in {
    val config = Config().withJdkPath(System.getProperty("java.home"))
    val cpg = code("""
     |public class Test {
     |    public static void foo(String s) {
     |        s.trim().isEmpty();
     |    }
     |}
     |""".stripMargin).withConfig(config)

    cpg.call.name("trim").methodFullName.head shouldBe "java.lang.String.trim:java.lang.String()"
    cpg.call.name("isEmpty").methodFullName.head shouldBe "java.lang.String.isEmpty:boolean()"
  }
}
