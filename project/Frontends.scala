
object Frontends {

  def c2cpgUrl = s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v${Versions.c2cpg}/c2cpg.zip"

  def fuzzyc2cpgUrl = s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v${Versions.fuzzyc2cpg}/fuzzy2cpg.zip"

  def js2cpgUrl = s"https://github.com/ShiftLeftSecurity/js2cpg/releases/download/v${Versions.js2cpg}/js2cpg.zip"

  def downloadC2CpgZip = SimpleCache.downloadMaybe(c2cpgUrl)
  def downloadFuzzyc2CpgZip = SimpleCache.downloadMaybe(fuzzyc2cpgUrl)
  def downloadJs2CpgZip = SimpleCache.downloadMaybe(js2cpgUrl)
}
