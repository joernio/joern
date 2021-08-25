
object Frontends {

  def downloadC2CpgZip =
    SimpleCache.downloadMaybe(s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v${Versions.cpg}/c2cpg.zip")

  def downloadFuzzyc2CpgZip =
    SimpleCache.downloadMaybe(s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v${Versions.cpg}/fuzzy2cpg.zip")

  def downloadJs2CpgZip =
    SimpleCache.downloadMaybe(s"https://github.com/ShiftLeftSecurity/js2cpg/releases/download/v${Versions.js2cpg}/js2cpg.zip")

}
