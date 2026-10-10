package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.shiftleft.semanticcpg.utils.FileUtil
import org.eclipse.cdt.internal.core.parser.scanner.CPreprocessor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URL
import java.net.URLClassLoader
import scala.util.Using

/** CDT shares its built-in macros between all parsers in the JVM and fills each one's token list on first use, after
  * storing it in the shared object. A parser on another thread can read it in between, empty or half filled. CdtParser
  * fills them when it is created, before any file is parsed in parallel; these tests check that it does.
  *
  * The test loads its own copy of CDT and of this package through [[CdtParserBuiltinMacroTests.IsolatingClassLoader]]:
  * its built-in macros start unfilled, as in a JVM where no parser has run yet, and nothing the test does reaches the
  * CDT used by other tests running in parallel in this JVM.
  */
class CdtParserBuiltinMacroTests extends AnyWordSpec with Matchers {

  import CdtParserBuiltinMacroTests.*

  private val builtinMacroNames = Seq("__STDC_VERSION__", "__STDC_HOSTED__", "__cplusplus", "__CDT_PARSER__")

  "CdtParser" should {

    "fill CDT's built-in macros when it is created" in {
      Using.resource(IsolatingClassLoader()) { loader =>
        // The isolated copy of CDT, not the one the rest of this JVM uses.
        loader.loadClass(classOf[CPreprocessor].getName) should not be classOf[CPreprocessor]
        val cdt = IsolatedCdt(loader)
        builtinMacroNames.flatMap(cdt.tokenImages) shouldBe empty

        FileUtil.usingTemporaryDirectory("cdtParserBuiltinMacros") { dir =>
          val config           = Config().withInputPath(dir.toString)
          val headerFileFinder = loader.loadClass(classOf[HeaderFileFinder].getName)
          val cdtParser        = loader.loadClass(classOf[CdtParser].getName)
          // The isolated copy, not the CdtParser the rest of this JVM uses.
          cdtParser should not be classOf[CdtParser]
          val finder = headerFileFinder.getConstructor(classOf[Config]).newInstance(config)
          cdtParser
            .getConstructor(classOf[Config], headerFileFinder, classOf[Option[?]])
            .newInstance(config, finder, None)
        }

        builtinMacroNames.foreach { name =>
          withClue(s"$name: ") {
            cdt.tokenImages(name) shouldBe Some(Seq(cdt.expansion(name)))
          }
        }
      }
    }
  }

}

object CdtParserBuiltinMacroTests {

  /** Loads CDT (except `CCorePlugin`) and `io.joern.c2cpg.parser` itself, so they get their own static state; every
    * other class comes from the parent. The parser package is loaded here too because it links against CDT's types.
    */
  private final class IsolatingClassLoader(urls: Array[URL], parent: ClassLoader) extends URLClassLoader(urls, parent) {

    // CCorePlugin stays shared: CPreprocessor uses it only for CDT's version and for logging, and a second copy would
    // set up a second logging context.
    private def isIsolated(name: String): Boolean =
      (name.startsWith("org.eclipse.cdt.") && !name.startsWith("org.eclipse.cdt.core.CCorePlugin")) ||
        name.startsWith("io.joern.c2cpg.parser.")

    override def loadClass(name: String, resolve: Boolean): Class[?] = getClassLoadingLock(name).synchronized {
      if (!isIsolated(name)) super.loadClass(name, resolve)
      else {
        val loaded = Option(findLoadedClass(name)).getOrElse {
          try { findClass(name) }
          catch { case _: ClassNotFoundException => super.loadClass(name, false) }
        }
        if (resolve) resolveClass(loaded)
        loaded
      }
    }
  }

  private object IsolatingClassLoader {
    def apply(): IsolatingClassLoader = {
      val urls = Seq(classOf[CPreprocessor], classOf[CdtParser]).map(_.getProtectionDomain.getCodeSource.getLocation)
      new IsolatingClassLoader(urls.distinct.toArray, classOf[CdtParserBuiltinMacroTests].getClassLoader)
    }
  }

  /** Reads the built-in macros of the CDT copy that `loader` holds, through reflection: its classes are not the classes
    * this test was compiled against.
    */
  private final class IsolatedCdt(loader: ClassLoader) {
    private val preprocessor     = loader.loadClass("org.eclipse.cdt.internal.core.parser.scanner.CPreprocessor")
    private val objectStyleMacro = loader.loadClass("org.eclipse.cdt.internal.core.parser.scanner.ObjectStyleMacro")
    private val tokenList        = loader.loadClass("org.eclipse.cdt.internal.core.parser.scanner.TokenList")
    private val token            = loader.loadClass("org.eclipse.cdt.internal.core.parser.scanner.Token")

    private val expansionTokens = accessible(objectStyleMacro.getDeclaredField("fExpansionTokens"))
    private val first           = accessible(tokenList.getDeclaredMethod("first"))
    private val next            = accessible(token.getMethod("getNext"))
    private val image           = accessible(token.getMethod("getImage"))
    private val getExpansion    = accessible(objectStyleMacro.getMethod("getExpansion"))

    private def accessible[T <: java.lang.reflect.AccessibleObject](member: T): T = {
      member.setAccessible(true)
      member
    }

    private def builtinMacro(name: String): AnyRef =
      accessible(preprocessor.getDeclaredField(name)).get(null)

    /** The macro's text, e.g. `199901L`. */
    def expansion(name: String): String =
      String(getExpansion.invoke(builtinMacro(name)).asInstanceOf[Array[Char]]).trim

    /** The images of the tokens in the macro's token list, or None while the list has not been created. */
    def tokenImages(name: String): Option[Seq[String]] =
      Option(expansionTokens.get(builtinMacro(name))).map { list =>
        Iterator
          .iterate(first.invoke(list))(t => next.invoke(t))
          .takeWhile(_ != null)
          .map(t => image.invoke(t).asInstanceOf[String])
          .toSeq
      }
  }

}
