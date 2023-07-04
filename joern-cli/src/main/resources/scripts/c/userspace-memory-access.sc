//> using file assertions.sc

// Find more information at http://www.makelinux.net/ldd3/ (Chapter 3)
// and https://www.kernel.org/doc/html/latest/core-api/mm-api.html
private val calls: Set[String] = Set(
  "copy_from_user",
  "copy_to_user",
  "put_user",
  "get_user",
  "__put_user", // __put_user and __get_user are unsafe, must verify target address with 'access_ok' first.
  "__get_user",
  "remap_pfn_range",
  "vm_iomap_memory"
)

@main def main(inputPath: String) = {
  importCode(inputPath)
  val calls0 = cpg.call.filter(call => calls.contains(call.name)).name

  val expected = Set("get_user")
  assertContains("calls", calls0, expected)
}
