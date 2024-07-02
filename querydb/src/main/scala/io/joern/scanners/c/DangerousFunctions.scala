package io.joern.scanners.c

import io.joern.scanners.*
import io.joern.console.*
import io.shiftleft.semanticcpg.language.*
import io.joern.macros.QueryMacros.*

object DangerousFunctions extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def getsUsed(): Query =
    Query.make(
      name = "call-to-gets",
      author = Crew.suchakra,
      title = "Dangerous function gets() used",
      description = """
        | Avoid `gets` function as it can lead to reads beyond buffer
        | boundary and cause
        | buffer overflows. Some secure alternatives are `fgets` and `gets_s`.
        |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        cpg.method("(?i)gets").callIn
      }),
      tags = List(QueryTags.badfn, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |int insecure_gets() {
          |  char str[DST_BUFFER_SIZE];
          |  gets(str);
          |  printf("%s", str);
          |  return 0;
          |}
          |
          |""".stripMargin),
        List("""
          |
          |int secure_gets() {
          |  FILE *fp;
          |  fp = fopen("file.txt" , "r");
          |  char str[DST_BUFFER_SIZE];
          |  fgets(str, DST_BUFFER_SIZE, fp);
          |  printf("%s", str);
          |  return 0;
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def argvUsedInPrintf(): Query =
    Query.make(
      name = "format-controlled-printf",
      author = Crew.suchakra,
      title = "Non-constant format string passed to printf/sprintf/vsprintf",
      description = """
        | Avoid user controlled format strings like "argv" in printf, sprintf and vsprintf 
        | functions as they can cause memory corruption. Some secure
        | alternatives are `snprintf` and `vsnprintf`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        val printfFns = cpg
          .method("(?i)printf")
          .callIn
          .whereNot(_.argument.order(1).isLiteral)
        val sprintsFns = cpg
          .method("(?i)(sprintf|vsprintf)")
          .callIn
          .whereNot(_.argument.order(2).isLiteral)
        printfFns ++ sprintsFns
      }),
      tags = List(QueryTags.badfn, QueryTags.default),
      codeExamples = CodeExamples(
        List(
          """
          |int insecure_printf() {
          |  printf(argv[1], 4242);
          |
          |}
          |""".stripMargin,
          """
          |
          |int insecure_sprintf() {
          |  char buffer [BUFF_SIZE];
          |  sprintf(buffer, argv[2], 4242);
          |}
          |
          |""".stripMargin
        ),
        List(
          """
          |
          |int secure_printf() {
          |  printf("Num: %d", 4242);
          |}
          |
          |""".stripMargin,
          """
          |
          |int secure_sprintf() {
          |  char buffer [BUFF_SIZE];
          |  snprintf(buffer, BUFF_SIZE, argv[2], 4242);
          |}
          |
          |""".stripMargin
        )
      )
    )

  @q
  def scanfUsed(): Query =
    Query.make(
      name = "call-to-scanf",
      author = Crew.suchakra,
      title = "Insecure function scanf() used",
      description = """
        | Avoid `scanf` function as it can lead to reads beyond buffer
        | boundary and cause buffer overflows. A secure alternative is `fgets`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method("(?i)scanf").callIn
      }),
      tags = List(QueryTags.badfn),
      codeExamples = CodeExamples(
        List("""
          |
          |int insecure_scanf() {
          |  char name[12];
          |  scanf("%s", name);
          |  printf("Hello %s!\n", name);
          |  return 0
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void secure_fgets(char *buf, int size, FILE *stream) {
          |    fgets(buf, size, stream);
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def strcatUsed(): Query =
    Query.make(
      name = "call-to-strcat",
      author = Crew.suchakra,
      title = "Dangerous functions `strcat` or `strncat` used",
      description = """
        | Avoid `strcat` or `strncat` functions. These can be used insecurely
        | causing non null-termianted strings leading to memory corruption.
        | A secure alternative is `strcat_s`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method("(?i)(strcat|strncat)").callIn
      }),
      tags = List(QueryTags.badfn),
      codeExamples = CodeExamples(
        List(
          """
          |
          |void insecure_strcat(char *dest, char *src) {
          |    strcat(dest, src);
          |}
          |
          |""".stripMargin,
          """
          |
          |int insecure_strncat() {
          |  char buf[BUF_SIZE];
          |  strncat(buf, another_buffer, BUF_SIZE - strlen(buf)); // remediation is (BUFF_SIZE - strlen(buf) - 1)
          |  return 0
          |}
          |
          |""".stripMargin
        ),
        List("""
          |
          |void secure_strcat_s(char *dest, rsize_t destsz, char *src) {
          |    strcat_s(dest, destsz, src);
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def strcpyUsed(): Query =
    Query.make(
      name = "call-to-strcpy",
      author = Crew.suchakra,
      title = "Dangerous functions `strcpy` or `strncpy` used",
      description = """
        | Avoid `strcpy` or `strncpy` function. `strcpy` does not check buffer
        | lengths.
        | A possible mitigation could be `strncpy` which could prevent
        | buffer overflows but does not null-terminate strings leading to
        | memory corruption. A secure alternative (on BSD) is `strlcpy`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method("(?i)(strcpy|strncpy)").callIn
      }),
      tags = List(QueryTags.badfn),
      codeExamples = CodeExamples(
        List(
          """
          |
          |void insecure_strcpy(char *dest, char *src) {
          |    strcpy(dest, src);
          |}
          |
          |""".stripMargin,
          """
          |
          |int insecure_strncpy() {
          |  char buf[BUF_SIZE];
          |  strncpy(buf, default_value, BUF_SIZE); // remediation is (BUFF_SIZE - 1)
          |  return 0
          |}
          |
          |""".stripMargin
        ),
        List("""
          |
          |void secure_strlcpy(char *dest, char *src, size_t size) {
          |    strlcpy(dest, src, size);
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def strtokUsed(): Query =
    Query.make(
      name = "call-to-strtok",
      author = Crew.suchakra,
      title = "Dangerous function strtok() used",
      description = """
        | Avoid `strtok` function as it modifies the original string in place
        | and appends a null character after each token. This makes the
        | original string unsafe. Suggested alternative is `strtok_r` with
        | `saveptr`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method("(?i)strtok").callIn
      }),
      tags = List(QueryTags.badfn, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |int insecure_strtok() {
          |  char *token;
          |  char *path = getenv("PATH");
          |  token = strtok(path, ":");
          |  puts(token);
          |  printf("PATH: %s\n", path); // original path string now has '/usr/bin\0' now and is insecure to use
          |  return 0;
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void secure_strtok_r(char *src, char *delim, char **saveptr) {
          |    strtok_r(src, delim, saveptr);
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def getwdUsed(): Query =
    Query.make(
      name = "call-to-getwd",
      author = Crew.claudiu,
      title = "Dangerous function getwd() used",
      description = """
        | Avoid the `getwd` function, it does not check buffer lengths.
        | Use `getcwd` instead, as it checks the buffer size.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method("(?i)getwd").callIn
      }),
      tags = List(QueryTags.badfn, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |int insecure_getwd() {
          |  char dir[12];
          |  getwd(dir);
          |  printf("Working directory:%s\n",dir);
          |  return 0;
          |}
          |
          |""".stripMargin),
        List("""
          |void secure_getcwd(char *buf, size_t len) {
          |    getcwd(buf, len);
          |}
          |""".stripMargin)
      )
    )
}
