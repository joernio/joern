package io.joern.javasrc2cpg.passes

import better.files.File
import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.Cpg

class ConfigFileCreationPass(cpg: Cpg) extends XConfigFileCreationPass(cpg) {

  override val configFileFilters: List[File => Boolean] = List(
    // JAVA_INTERNAL
    extensionFilter(".properties"),
    // JSP
    extensionFilter(".jsp"),
    // Velocity files, see https://velocity.apache.org
    extensionFilter(".vm"),
    // For Terraform secrets
    extensionFilter(".tf"),
    extensionFilter(".tfvars"),
    // PLAY
    pathEndFilter("routes"),
    pathEndFilter("application.conf"),
    // SERVLET
    pathEndFilter("web.xml"),
    // JSF
    pathEndFilter("faces-config.xml"),
    // STRUTS
    pathEndFilter("struts.xml"),
    // DIRECT WEB REMOTING
    pathEndFilter("dwr.xml"),
    // MYBATIS
    mybatisFilter,
    // BUILD SYSTEM
    pathEndFilter("build.gradle"),
    pathEndFilter("build.gradle.kts"),
    // ANDROID
    pathEndFilter("AndroidManifest.xml"),
    // SPRING
    extensionFilter(".yaml"),
    extensionFilter(".yml")
  )

  private def mybatisFilter(file: File): Boolean = {
    file.canonicalPath.contains("batis") && file.extension.contains(".xml")
  }

}
