package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

class ParameterOutTests extends JavaSrcCode2CpgFixture(withOssDataflow = true) {

  "allow traversing through a parameter out" in {
    val cpg = code("""
        |class Foo {
        |    public static InputStream getStream(String resource) throws NoResourceException {
        |        return getResourceObj(resource).getStream();
        |    }
        |    public static Resource getResourceObj(String path) {
        |        if (isNotBlank(path, "aaa")) {
        |            if (path.startsWith(URLUtil.FILE_URL_PREFIX) || FileUtil.isAbsolutePath(path)) {
        |                return new FileResource(path);
        |            }
        |        }
        |        return new ClassPathResource(path);
        |    }
        |    public static boolean isNotBlank(CharSequence str, String aaa) {
        |        return false == isBlank(str);
        |    }
        |}
        |""".stripMargin)

    def src = cpg.parameter("str")
    def snk = cpg.ret.lineNumber(4)
    snk.reachableByFlows(src).p.foreach(println)
  }

}
