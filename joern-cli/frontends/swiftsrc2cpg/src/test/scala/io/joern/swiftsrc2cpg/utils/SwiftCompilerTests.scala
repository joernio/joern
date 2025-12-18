package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path}

class SwiftCompilerTests extends AnyWordSpec with Matchers {

  private object SwiftCompilerTestsFixture {
    private val PackageSwiftContent: String =
      """
        |// swift-tools-version: 5.10
        |
        |import PackageDescription
        |
        |let package = Package(
        |  name: "SwiftHelloWorld",
        |  platforms: [
        |    .macOS(.v10_15)
        |  ],
        |  products: [
        |    .library(name: "SwiftHelloWorldLib", targets: ["SwiftHelloWorldLib"])
        |  ],
        |  targets: [
        |    .target(
        |      name: "SwiftHelloWorldLib",
        |      swiftSettings: [
        |        .unsafeFlags(["-warnings-as-errors"])
        |      ]
        |    ),
        |    .executableTarget(
        |      name: "SwiftHelloWorld",
        |      dependencies: [ "SwiftHelloWorldLib" ],
        |      swiftSettings: [
        |        .unsafeFlags(["-warnings-as-errors"])
        |      ]
        |    )
        |  ]
        |)
        |""".stripMargin

    private val HelloWorldSwiftContent =
      """
        |public class HelloWorld {
        |
        |	private let greeting: String = "Hello World"
        |	private let suffix: String = "!"
        |
        |   var _members: Int = 2
        |   var members: Int {
        |      get {
        |        return _members
        |      }
        |      set (newVal) {
        |        if newVal >= 2 {
        |          _members = newVal
        |        }
        |      }
        |   }
        |
        |   var bar2: Int {
        |     willSet {}
        |     didSet {}
        |   }
        |
        |	public init() { self.bar2 = 1 }
        |
        |	public func greet(from name: String) {
        |		print(greeting + " from " + name + suffix)
        |	}
        |
        |}
        |""".stripMargin

    val JsonStringHelloWorldSwift =
      """{"_kind":"source_file","filename":"HelloWorldSwift.swift","compiler_version":{"_kind":"compiler_version","major":"6","minor":"1","full":"Swift version 6.1.2 (swift-6.1.2-RELEASE)"},"items":[{"_kind":"import_decl","range":{"start":1,"end":8},"module_path":["Foundation"]},{"_kind":"class_decl","usr":"s:18SwiftHelloWorldLib0bC0C","range":{"start":27,"end":236},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"HelloWorld"}},"interface_type":"$s18SwiftHelloWorldLib0bC0CmD","access":"public","non_resilient":true,"attrs":[{"_kind":"access_control_attr","range":{"start":20,"end":20},"access_level":"public"}],"members":[{"_kind":"pattern_binding_decl","range":{"start":56,"end":79},"pattern_entries":[{"_kind":"pattern_entry","pattern":{"_kind":"pattern_typed","type":"$sSSD","sub_pattern":{"_kind":"pattern_named","type":"$sSSD","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"greeting"}}}},"original_init":{"_kind":"string_literal_expr","type":"$sSSD","range":{"start":79,"end":79},"encoding":"utf8","value":"Hello World","builtin_initializer":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:SS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfc","type_usr":"$s21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcD"}},"processed_init":{"_kind":"string_literal_expr","type":"$sSSD","range":{"start":79,"end":79},"encoding":"utf8","value":"Hello World","builtin_initializer":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:SS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfc","type_usr":"$s21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcD"}}}]},{"_kind":"var_decl","usr":"s:18SwiftHelloWorldLib0bC0C8greeting33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp","range":{"start":60,"end":60},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"greeting"}},"interface_type":"$sSSD","access":"private","let":true,"readImpl":"stored","immutable":true,"accessors":[{"_kind":"accessor_decl","usr":"s:18SwiftHelloWorldLib0bC0C8greeting33_C6D5E4A96804CD03B7512662F178D1D8LLSSvg","implicit":true,"range":{"start":60,"end":60},"interface_type":"$sySSyc18SwiftHelloWorldLib0bC0CcD","access":"private","final":true,"get":true,"for":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"greeting"}},"implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}},"interface_type":"$s18SwiftHelloWorldLib0bC0CD"},"result":"$sSSD","thrown_type":"","params":{"_kind":"parameter_list"},"body":{"_kind":"brace_stmt","implicit":true,"range":{"start":60,"end":60},"elements":[{"_kind":"return_stmt","implicit":true,"result":{"_kind":"member_ref_expr","implicit":true,"type":"$sSSD","decl":{"_kind":"decl_ref","base_name":"greeting","decl_usr":"s:18SwiftHelloWorldLib0bC0C8greeting33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp","type_usr":"$sSSD"},"direct_to_storage":true,"base":{"_kind":"declref_expr","implicit":true,"type":"$s18SwiftHelloWorldLib0bC0CD","decl":{"_kind":"decl_ref","base_name":"self","decl_usr":"","type_usr":"$s18SwiftHelloWorldLib0bC0CD"},"function_ref":"unapplied"}}}]}}]},{"_kind":"pattern_binding_decl","range":{"start":102,"end":123},"pattern_entries":[{"_kind":"pattern_entry","pattern":{"_kind":"pattern_typed","type":"$sSSD","sub_pattern":{"_kind":"pattern_named","type":"$sSSD","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"suffix"}}}},"original_init":{"_kind":"string_literal_expr","type":"$sSSD","range":{"start":123,"end":123},"encoding":"utf8","value":"!","builtin_initializer":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:SS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfc","type_usr":"$s21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcD"}},"processed_init":{"_kind":"string_literal_expr","type":"$sSSD","range":{"start":123,"end":123},"encoding":"utf8","value":"!","builtin_initializer":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:SS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfc","type_usr":"$s21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcD"}}}]},{"_kind":"var_decl","usr":"s:18SwiftHelloWorldLib0bC0C6suffix33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp","range":{"start":106,"end":106},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"suffix"}},"interface_type":"$sSSD","access":"private","let":true,"readImpl":"stored","immutable":true,"accessors":[{"_kind":"accessor_decl","usr":"s:18SwiftHelloWorldLib0bC0C6suffix33_C6D5E4A96804CD03B7512662F178D1D8LLSSvg","implicit":true,"range":{"start":106,"end":106},"interface_type":"$sySSyc18SwiftHelloWorldLib0bC0CcD","access":"private","final":true,"get":true,"for":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"suffix"}},"implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}},"interface_type":"$s18SwiftHelloWorldLib0bC0CD"},"result":"$sSSD","thrown_type":"","params":{"_kind":"parameter_list"},"body":{"_kind":"brace_stmt","implicit":true,"range":{"start":106,"end":106},"elements":[{"_kind":"return_stmt","implicit":true,"result":{"_kind":"member_ref_expr","implicit":true,"type":"$sSSD","decl":{"_kind":"decl_ref","base_name":"suffix","decl_usr":"s:18SwiftHelloWorldLib0bC0C6suffix33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp","type_usr":"$sSSD"},"direct_to_storage":true,"base":{"_kind":"declref_expr","implicit":true,"type":"$s18SwiftHelloWorldLib0bC0CD","decl":{"_kind":"decl_ref","base_name":"self","decl_usr":"","type_usr":"$s18SwiftHelloWorldLib0bC0CD"},"function_ref":"unapplied"}}}]}}]},{"_kind":"constructor_decl","usr":"s:18SwiftHelloWorldLib0bC0CACycfc","range":{"start":136,"end":144},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","special":"init"}},"interface_type":"$sy18SwiftHelloWorldLib0bC0CycACmcD","access":"public","designated":true,"implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}}},"thrown_type":"","params":{"_kind":"parameter_list","range":{"start":140,"end":141}},"body":{"_kind":"brace_stmt","range":{"start":143,"end":144},"elements":[{"_kind":"return_stmt","implicit":true,"range":{"start":144,"end":144}}]}},{"_kind":"func_decl","usr":"s:18SwiftHelloWorldLib0bC0C5greet4fromySS_tF","range":{"start":155,"end":233},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"greet"},"args":["from"]},"interface_type":"$syySS_tc18SwiftHelloWorldLib0bC0CcD","access":"public","implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}},"interface_type":"$s18SwiftHelloWorldLib0bC0CD"},"result":"$sytD","thrown_type":"","params":{"_kind":"parameter_list","range":{"start":165,"end":183},"params":[{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"name"}},"apiName":"from","interface_type":"$sSSD"}]},"body":{"_kind":"brace_stmt","range":{"start":185,"end":233},"elements":[{"_kind":"call_expr","type":"$sytD","range":{"start":189,"end":230},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","type":"$s_9separator10terminatoryypd_S2StcD","range":{"start":189,"end":189},"decl":{"_kind":"decl_ref","base_name":"print","decl_usr":"s:s5print_9separator10terminatoryypd_S2StF","type_usr":"$s_9separator10terminatoryypd_S2StcD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","labels":"_:separator:terminator:","args":[{"_kind":"argument","expr":{"_kind":"vararg_expansion_expr","implicit":true,"type":"$sypXSaD","range":{"start":195,"end":224},"sub_expr":{"_kind":"array_expr","implicit":true,"type":"$sypXSaD","range":{"start":195,"end":224},"elements":[{"_kind":"erasure_expr","implicit":true,"type":"$sypD","range":{"start":195,"end":224},"conformances":[{"_kind":"builtin_conformance","type":"$sSSD","protocol":"s:s8CopyableP"},{"_kind":"builtin_conformance","type":"$sSSD","protocol":"s:s9EscapableP"}],"sub_expr":{"_kind":"binary_expr","type":"$sSSD","range":{"start":195,"end":224},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"dot_syntax_call_expr","implicit":true,"type":"$syS2S_SStcD","range":{"start":222,"end":222},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","type":"$syS2S_SStcSSmcD","range":{"start":222,"end":222},"decl":{"_kind":"decl_ref","base_name":"+","decl_usr":"s:SS1poiyS2S_SStFZ","type_usr":"$syS2S_SStcD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"type_expr","implicit":true,"type":"$sSSmD","range":{"start":222,"end":222}}}]}},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"binary_expr","type":"$sSSD","range":{"start":195,"end":217},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"dot_syntax_call_expr","implicit":true,"type":"$syS2S_SStcD","range":{"start":215,"end":215},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","type":"$syS2S_SStcSSmcD","range":{"start":215,"end":215},"decl":{"_kind":"decl_ref","base_name":"+","decl_usr":"s:SS1poiyS2S_SStFZ","type_usr":"$syS2S_SStcD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"type_expr","implicit":true,"type":"$sSSmD","range":{"start":215,"end":215}}}]}},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"binary_expr","type":"$sSSD","range":{"start":195,"end":206},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"dot_syntax_call_expr","implicit":true,"type":"$syS2S_SStcD","range":{"start":204,"end":204},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","type":"$syS2S_SStcSSmcD","range":{"start":204,"end":204},"decl":{"_kind":"decl_ref","base_name":"+","decl_usr":"s:SS1poiyS2S_SStFZ","type_usr":"$syS2S_SStcD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"type_expr","implicit":true,"type":"$sSSmD","range":{"start":204,"end":204}}}]}},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"member_ref_expr","type":"$sSSD","range":{"start":195,"end":195},"decl":{"_kind":"decl_ref","base_name":"greeting","decl_usr":"s:18SwiftHelloWorldLib0bC0C8greeting33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp","type_usr":"$sSSD"},"base":{"_kind":"declref_expr","implicit":true,"type":"$s18SwiftHelloWorldLib0bC0CD","range":{"start":195,"end":195},"decl":{"_kind":"decl_ref","base_name":"self","decl_usr":"","type_usr":"$s18SwiftHelloWorldLib0bC0CD"},"function_ref":"unapplied"}}},{"_kind":"argument","expr":{"_kind":"string_literal_expr","type":"$sSSD","range":{"start":206,"end":206},"encoding":"utf8","value":" from ","builtin_initializer":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:SS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfc","type_usr":"$s21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcD"}}}]}}},{"_kind":"argument","expr":{"_kind":"declref_expr","type":"$sSSD","range":{"start":217,"end":217},"decl":{"_kind":"decl_ref","base_name":"name","decl_usr":"","type_usr":"$sSSD"},"function_ref":"unapplied"}}]}}},{"_kind":"argument","expr":{"_kind":"member_ref_expr","type":"$sSSD","range":{"start":224,"end":224},"decl":{"_kind":"decl_ref","base_name":"suffix","decl_usr":"s:18SwiftHelloWorldLib0bC0C6suffix33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp","type_usr":"$sSSD"},"base":{"_kind":"declref_expr","implicit":true,"type":"$s18SwiftHelloWorldLib0bC0CD","range":{"start":224,"end":224},"decl":{"_kind":"decl_ref","base_name":"self","decl_usr":"","type_usr":"$s18SwiftHelloWorldLib0bC0CD"},"function_ref":"unapplied"}}}]}}}]}}},{"_kind":"argument","label":"separator","expr":{"_kind":"default_argument_expr","implicit":true,"type":"$sSSD","range":{"start":194,"end":194},"default_args_owner":{"_kind":"decl_ref","base_name":"print","decl_usr":"s:s5print_9separator10terminatoryypd_S2StF","type_usr":"$s_9separator10terminatoryypd_S2StcD"},"param":"1"}},{"_kind":"argument","label":"terminator","expr":{"_kind":"default_argument_expr","implicit":true,"type":"$sSSD","range":{"start":194,"end":194},"default_args_owner":{"_kind":"decl_ref","base_name":"print","decl_usr":"s:s5print_9separator10terminatoryypd_S2StF","type_usr":"$s_9separator10terminatoryypd_S2StcD"},"param":"2"}}]}}]}},{"_kind":"destructor_decl","usr":"s:18SwiftHelloWorldLib0bC0Cfd","implicit":true,"range":{"start":33,"end":33},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","special":"deinit"}},"interface_type":"$syyyc18SwiftHelloWorldLib0bC0CcD","access":"public","implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}}},"thrown_type":"","params":{"_kind":"parameter_list"},"body":{"_kind":"brace_stmt","implicit":true,"range":{"start":33,"end":33}}}]}]}"""

    val JsonStringMain =
      """{"_kind":"source_file","filename":"Main.swift","compiler_version":{"_kind":"compiler_version","major":"6","minor":"1","full":"Swift version 6.1.2 (swift-6.1.2-RELEASE)"},"items":[{"_kind":"import_decl","range":{"start":1,"end":8},"module_path":["Foundation"]},{"_kind":"import_decl","range":{"start":19,"end":26},"module_path":["SwiftHelloWorldLib"]},{"_kind":"struct_decl","usr":"s:15SwiftHelloWorld4MainV","range":{"start":52,"end":157},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"Main"}},"interface_type":"$s15SwiftHelloWorld4MainVmD","access":"internal","non_resilient":true,"attrs":[{"_kind":"main_type_attr","range":{"start":46,"end":47}}],"members":[{"_kind":"func_decl","usr":"s:15SwiftHelloWorld4MainV4mainyyFZ","range":{"start":67,"end":154},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"main"}},"interface_type":"$syyyc15SwiftHelloWorld4MainVmcD","access":"internal","static":true,"implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}}},"result":"$ss4VoidaD","thrown_type":"","params":{"_kind":"parameter_list","range":{"start":83,"end":84}},"body":{"_kind":"brace_stmt","range":{"start":94,"end":154},"elements":[{"_kind":"pattern_binding_decl","range":{"start":98,"end":123},"pattern_entries":[{"_kind":"pattern_entry","pattern":{"_kind":"pattern_named","type":"$s18SwiftHelloWorldLib0bC0CD","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"greeter"}}},"original_init":{"_kind":"call_expr","type":"$s18SwiftHelloWorldLib0bC0CD","range":{"start":112,"end":123},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"constructor_ref_call_expr","type":"$s18SwiftHelloWorldLib0bC0CycD","range":{"start":112,"end":112},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","implicit":true,"type":"$sy18SwiftHelloWorldLib0bC0CycACmcD","range":{"start":112,"end":112},"decl":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:18SwiftHelloWorldLib0bC0CACycfc","type_usr":"$s18SwiftHelloWorldLib0bC0CycD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"type_expr","type":"$s18SwiftHelloWorldLib0bC0CmD","range":{"start":112,"end":112}}}]}},"args":{"_kind":"argument_list"}},"processed_init":{"_kind":"call_expr","type":"$s18SwiftHelloWorldLib0bC0CD","range":{"start":112,"end":123},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"constructor_ref_call_expr","type":"$s18SwiftHelloWorldLib0bC0CycD","range":{"start":112,"end":112},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","implicit":true,"type":"$sy18SwiftHelloWorldLib0bC0CycACmcD","range":{"start":112,"end":112},"decl":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:18SwiftHelloWorldLib0bC0CACycfc","type_usr":"$s18SwiftHelloWorldLib0bC0CycD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"type_expr","type":"$s18SwiftHelloWorldLib0bC0CmD","range":{"start":112,"end":112}}}]}},"args":{"_kind":"argument_list"}}}]},{"_kind":"var_decl","range":{"start":102,"end":102},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"greeter"}},"interface_type":"$s18SwiftHelloWorldLib0bC0CD","access":"private","let":true,"readImpl":"stored","immutable":true},{"_kind":"call_expr","type":"$sytD","range":{"start":127,"end":151},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"dot_syntax_call_expr","type":"$s4fromySS_tcD","range":{"start":127,"end":135},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"declref_expr","type":"$syySS_tc18SwiftHelloWorldLib0bC0CcD","range":{"start":135,"end":135},"decl":{"_kind":"decl_ref","base_name":"greet","decl_usr":"s:18SwiftHelloWorldLib0bC0C5greet4fromySS_tF","type_usr":"$s4fromySS_tcD"},"function_ref":"single_apply"},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"declref_expr","type":"$s18SwiftHelloWorldLib0bC0CD","range":{"start":127,"end":127},"decl":{"_kind":"decl_ref","base_name":"greeter","decl_usr":"","type_usr":"$s18SwiftHelloWorldLib0bC0CD"},"function_ref":"unapplied"}}]}},"args":{"_kind":"argument_list","labels":"from:","args":[{"_kind":"argument","label":"from","expr":{"_kind":"string_literal_expr","type":"$sSSD","range":{"start":147,"end":147},"encoding":"utf8","value":"me","builtin_initializer":{"_kind":"decl_ref","base_name":"init","decl_usr":"s:SS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfc","type_usr":"$s21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcD"}}}]}}]}},{"_kind":"func_decl","usr":"s:15SwiftHelloWorld4MainV5$mainyyFZ","implicit":true,"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"$main"}},"interface_type":"$syyyc15SwiftHelloWorld4MainVmcD","access":"internal","static":true,"implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}}},"result":"$sytD","thrown_type":"","params":{"_kind":"parameter_list"},"body":{"_kind":"brace_stmt","implicit":true,"range":{"start":46,"end":46},"elements":[{"_kind":"return_stmt","implicit":true,"range":{"start":46,"end":46},"result":{"_kind":"call_expr","implicit":true,"type":"$ss4VoidaD","range":{"start":46,"end":46},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"dot_syntax_call_expr","implicit":true,"type":"$syyScMYccD","range":{"start":46,"end":46},"nothrow":true,"isolation_crossing":"none","fn":{"_kind":"function_conversion_expr","implicit":true,"type":"$syyyScMYcc15SwiftHelloWorld4MainVmcD","range":{"start":46,"end":46},"sub_expr":{"_kind":"declref_expr","implicit":true,"type":"$syyyc15SwiftHelloWorld4MainVmcD","range":{"start":46,"end":46},"decl":{"_kind":"decl_ref","base_name":"main","decl_usr":"s:15SwiftHelloWorld4MainV4mainyyFZ","type_usr":"$syycD"},"function_ref":"double_apply"}},"args":{"_kind":"argument_list","implicit":true,"args":[{"_kind":"argument","expr":{"_kind":"type_expr","implicit":true,"type":"$s15SwiftHelloWorld4MainVmD"}}]}},"args":{"_kind":"argument_list","implicit":true}}}]}},{"_kind":"constructor_decl","usr":"s:15SwiftHelloWorld4MainVACycfc","implicit":true,"range":{"start":59,"end":59},"name":{"_kind":"decl_name","base_name":{"_kind":"base_name","special":"init"}},"interface_type":"$sy15SwiftHelloWorld4MainVycACmcD","access":"internal","designated":true,"implicit_self_decl":{"_kind":"parameter","name":{"_kind":"decl_name","base_name":{"_kind":"base_name","name":"self"}}},"thrown_type":"","params":{"_kind":"parameter_list"},"body":{"_kind":"brace_stmt","implicit":true,"range":{"start":59,"end":59},"elements":[{"_kind":"return_stmt","implicit":true,"range":{"start":59,"end":59}}]}}]}]}"""

    private val MainSwiftContent: String =
      """
        |import SwiftHelloWorldLib
        |
        |@main
        |struct Main {
        |	static func main() -> Void {
        |		let greeter = HelloWorld()
        |		greeter.greet(from: "me")
        |	}
        |
        |}
        |""".stripMargin

    val SwiftInvocation: String =
      """
        |/usr/bin/swiftc
        | -module-name SwiftHelloWorld
        | -emit-dependencies
        | -emit-module
        | -emit-module-path /tmp/project/.build/debug/Modules/SwiftHelloWorld.swiftmodule
        | -output-file-map /tmp/project/.build/debug/SwiftHelloWorld.build/output-file-map.json
        | -parse-as-library -incremental -c @/tmp/project/.build/debug/SwiftHelloWorld.build/sources
        | -I /tmp/project/.build/debug/Modules
        | -target x86_64-unknown-windows-msvc
        | -v
        | -enable-batch-mode
        | -index-store-path /tmp/project/.build/debug/index/store
        | -Onone
        | -enable-testing
        | -j16
        | -DSWIFT_PACKAGE
        | -DDEBUG
        | -module-cache-path /tmp/project/.build/debug/ModuleCache
        | -parseable-output
        | -parse-as-library
        | -static
        | -swift-version 5
        | -sdk /some/SDK/
        | -libc MD
        | -I /path/to/XCTest/usr/lib/swift/windows
        | -I /path/to/XCTest/usr/lib/swift/windows/x86_64
        | -L /path/to/XCTest/usr/lib/swift/windows/x86_64
        | -I /path/to/Testing/usr/lib/swift/windows
        | -L /path/to/Testing/usr/lib/swift/windows/x86_64
        | -use-ld=lld -g -use-ld=lld -Xcc -D_MT -Xcc -D_DLL -Xcc -Xclang -Xcc --dependent-lib=msvcrt -Xcc -gdwarf -package-name swiftastgen""".stripMargin
        .replace("\n", "")

    val SwiftInvocationDependency: String =
      """
        |/usr/bin/swiftc
        | -module-name Foundation
        | -emit-dependencies
        | -emit-module
        | -emit-module-path /tmp/project/.build/debug/Modules/SwiftHelloWorld.swiftmodule
        | -output-file-map /tmp/project/.build/debug/SwiftHelloWorld.build/output-file-map.json
        | -parse-as-library -incremental -c @/tmp/project/.build/debug/SwiftHelloWorld.build/sources
        | -I /tmp/project/.build/debug/Modules
        | -target x86_64-unknown-windows-msvc
        | -v
        | -enable-batch-mode
        | -index-store-path /tmp/project/.build/debug/index/store
        | -Onone
        | -enable-testing
        | -j16
        | -DSWIFT_PACKAGE
        | -DDEBUG
        | -module-cache-path /tmp/project/.build/debug/ModuleCache
        | -parseable-output
        | -parse-as-library
        | -static
        | -swift-version 5
        | -sdk /some/SDK/
        | -libc MD
        | -I /path/to/XCTest/usr/lib/swift/windows
        | -I /path/to/XCTest/usr/lib/swift/windows/x86_64
        | -L /path/to/XCTest/usr/lib/swift/windows/x86_64
        | -I /path/to/Testing/usr/lib/swift/windows
        | -L /path/to/Testing/usr/lib/swift/windows/x86_64
        | -use-ld=lld -g -use-ld=lld -Xcc -D_MT -Xcc -D_DLL -Xcc -Xclang -Xcc --dependent-lib=msvcrt -Xcc -gdwarf -package-name swiftastgen""".stripMargin
        .replace("\n", "")

    val ExpectedSwiftInvocations: Seq[Seq[String]] = Seq(
      Seq(
        "/usr/bin/swiftc",
        "-module-name",
        "SwiftHelloWorld",
        "-parse-as-library",
        "-c",
        "@/tmp/project/.build/debug/SwiftHelloWorld.build/sources",
        "-I",
        "/tmp/project/.build/debug/Modules",
        "-target",
        "x86_64-unknown-windows-msvc",
        "-enable-batch-mode",
        "-index-store-path",
        "/tmp/project/.build/debug/index/store",
        "-Onone",
        "-enable-testing",
        "-j16",
        "-DSWIFT_PACKAGE",
        "-DDEBUG",
        "-module-cache-path",
        "/tmp/project/.build/debug/ModuleCache",
        "-parse-as-library",
        "-static",
        "-swift-version",
        "5",
        "-sdk",
        "/some/SDK",
        "-libc",
        "MD",
        "-I",
        "/path/to/XCTest/usr/lib/swift/windows",
        "-I",
        "/path/to/XCTest/usr/lib/swift/windows/x86_64",
        "-L",
        "/path/to/XCTest/usr/lib/swift/windows/x86_64",
        "-I",
        "/path/to/Testing/usr/lib/swift/windows",
        "-L",
        "/path/to/Testing/usr/lib/swift/windows/x86_64",
        "-use-ld=lld",
        "-g",
        "-use-ld=lld",
        "-Xcc",
        "-D_MT",
        "-Xcc",
        "-D_DLL",
        "-Xcc",
        "-Xclang",
        "-Xcc",
        "--dependent-lib=msvcrt",
        "-Xcc",
        "-gdwarf",
        "-package-name",
        "swiftastgen",
        "-suppress-warnings",
        "-dump-ast",
        "-dump-ast-format",
        "json"
      )
    )

    val ExpectedMapping: Seq[(String, String, (Int, Int), Some[String], Option[String])] = List(
      (
        "HelloWorldSwift.swift",
        "accessor_decl",
        (106, 106),
        Some("Swift.String"),
        Some("SwiftHelloWorldLib.HelloWorld.suffix.getter:Swift.String")
      ),
      (
        "HelloWorldSwift.swift",
        "accessor_decl",
        (60, 60),
        Some("Swift.String"),
        Some("SwiftHelloWorldLib.HelloWorld.greeting.getter:Swift.String")
      ),
      ("HelloWorldSwift.swift", "array_expr", (195, 225), Some("[Any]"), None),
      ("HelloWorldSwift.swift", "binary_expr", (195, 207), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "binary_expr", (195, 218), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "binary_expr", (195, 225), Some("Swift.String"), None),
      (
        "HelloWorldSwift.swift",
        "call_expr",
        (189, 231),
        Some("()"),
        Some("Swift.print(_:Any...,separator:Swift.String,terminator:Swift.String)->()")
      ),
      (
        "HelloWorldSwift.swift",
        "class_decl",
        (20, 237),
        Some("SwiftHelloWorldLib.HelloWorld"),
        Some("SwiftHelloWorldLib.HelloWorld")
      ),
      (
        "HelloWorldSwift.swift",
        "constructor_decl",
        (136, 145),
        Some("(SwiftHelloWorldLib.HelloWorld.Type)->()->SwiftHelloWorldLib.HelloWorld"),
        Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
      ),
      (
        "HelloWorldSwift.swift",
        "declref_expr",
        (189, 189),
        Some("(_:Any...,separator:Swift.String,terminator:Swift.String)->()"),
        None
      ),
      ("HelloWorldSwift.swift", "declref_expr", (195, 195), Some("SwiftHelloWorldLib.HelloWorld"), None),
      (
        "HelloWorldSwift.swift",
        "declref_expr",
        (204, 204),
        Some("(Swift.String.Type)->(Swift.String,Swift.String)->Swift.String"),
        None
      ),
      (
        "HelloWorldSwift.swift",
        "declref_expr",
        (215, 215),
        Some("(Swift.String.Type)->(Swift.String,Swift.String)->Swift.String"),
        None
      ),
      ("HelloWorldSwift.swift", "declref_expr", (217, 217), Some("Swift.String"), None),
      (
        "HelloWorldSwift.swift",
        "declref_expr",
        (222, 222),
        Some("(Swift.String.Type)->(Swift.String,Swift.String)->Swift.String"),
        None
      ),
      ("HelloWorldSwift.swift", "declref_expr", (224, 224), Some("SwiftHelloWorldLib.HelloWorld"), None),
      ("HelloWorldSwift.swift", "default_argument_expr", (194, 194), Some("Swift.String"), None),
      (
        "HelloWorldSwift.swift",
        "destructor_decl",
        (33, 33),
        Some("(SwiftHelloWorldLib.HelloWorld)->()->()"),
        Some("SwiftHelloWorldLib.HelloWorld.deinit")
      ),
      (
        "HelloWorldSwift.swift",
        "dot_syntax_call_expr",
        (204, 204),
        Some("(Swift.String,Swift.String)->Swift.String"),
        Some("Swift.String.+infix(Swift.String,Swift.String)->Swift.String")
      ),
      (
        "HelloWorldSwift.swift",
        "dot_syntax_call_expr",
        (215, 215),
        Some("(Swift.String,Swift.String)->Swift.String"),
        Some("Swift.String.+infix(Swift.String,Swift.String)->Swift.String")
      ),
      (
        "HelloWorldSwift.swift",
        "dot_syntax_call_expr",
        (222, 222),
        Some("(Swift.String,Swift.String)->Swift.String"),
        Some("Swift.String.+infix(Swift.String,Swift.String)->Swift.String")
      ),
      ("HelloWorldSwift.swift", "erasure_expr", (195, 225), Some("Any"), None),
      (
        "HelloWorldSwift.swift",
        "func_decl",
        (155, 234),
        Some("()"),
        Some("SwiftHelloWorldLib.HelloWorld.greet(from:Swift.String)->()")
      ),
      (
        "HelloWorldSwift.swift",
        "member_ref_expr",
        (195, 195),
        Some("Swift.String"),
        Some("SwiftHelloWorldLib.HelloWorld.greeting:Swift.String")
      ),
      (
        "HelloWorldSwift.swift",
        "member_ref_expr",
        (224, 224),
        Some("Swift.String"),
        Some("SwiftHelloWorldLib.HelloWorld.suffix:Swift.String")
      ),
      ("HelloWorldSwift.swift", "parameter", (106, 106), Some("SwiftHelloWorldLib.HelloWorld"), None),
      ("HelloWorldSwift.swift", "parameter", (155, 234), Some("SwiftHelloWorldLib.HelloWorld"), None),
      ("HelloWorldSwift.swift", "parameter", (165, 184), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "parameter", (60, 60), Some("SwiftHelloWorldLib.HelloWorld"), None),
      ("HelloWorldSwift.swift", "string_literal_expr", (123, 123), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "string_literal_expr", (206, 206), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "string_literal_expr", (79, 79), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "type_expr", (204, 204), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "type_expr", (215, 215), Some("Swift.String"), None),
      ("HelloWorldSwift.swift", "type_expr", (222, 222), Some("Swift.String"), None),
      (
        "HelloWorldSwift.swift",
        "var_decl",
        (106, 106),
        Some("Swift.String"),
        Some("SwiftHelloWorldLib.HelloWorld.suffix:Swift.String")
      ),
      (
        "HelloWorldSwift.swift",
        "var_decl",
        (60, 60),
        Some("Swift.String"),
        Some("SwiftHelloWorldLib.HelloWorld.greeting:Swift.String")
      ),
      ("HelloWorldSwift.swift", "vararg_expansion_expr", (195, 225), Some("[Any]"), None),
      (
        "Main.swift",
        "call_expr",
        (112, 124),
        Some("SwiftHelloWorldLib.HelloWorld"),
        Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
      ),
      (
        "Main.swift",
        "call_expr",
        (127, 152),
        Some("()"),
        Some("SwiftHelloWorldLib.HelloWorld.greet(from:Swift.String)->()")
      ),
      ("Main.swift", "call_expr", (46, 46), Some("Swift.Void"), Some("SwiftHelloWorld.Main.main()->()")),
      (
        "Main.swift",
        "constructor_decl",
        (59, 59),
        Some("(SwiftHelloWorld.Main.Type)->()->SwiftHelloWorld.Main"),
        Some("SwiftHelloWorld.Main.init()->SwiftHelloWorld.Main")
      ),
      (
        "Main.swift",
        "constructor_ref_call_expr",
        (112, 112),
        Some("()->SwiftHelloWorldLib.HelloWorld"),
        Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
      ),
      (
        "Main.swift",
        "declref_expr",
        (112, 112),
        Some("(SwiftHelloWorldLib.HelloWorld.Type)->()->SwiftHelloWorldLib.HelloWorld"),
        None
      ),
      ("Main.swift", "declref_expr", (127, 127), Some("SwiftHelloWorldLib.HelloWorld"), None),
      ("Main.swift", "declref_expr", (135, 135), Some("(SwiftHelloWorldLib.HelloWorld)->(Swift.String)->()"), None),
      ("Main.swift", "declref_expr", (46, 46), Some("(SwiftHelloWorld.Main.Type)->()->()"), None),
      (
        "Main.swift",
        "dot_syntax_call_expr",
        (127, 138),
        Some("(from:Swift.String)->()"),
        Some("SwiftHelloWorldLib.HelloWorld.greet(from:Swift.String)->()")
      ),
      (
        "Main.swift",
        "dot_syntax_call_expr",
        (46, 46),
        Some("@Swift.MainActor()->()"),
        Some("SwiftHelloWorld.Main.main()->()")
      ),
      ("Main.swift", "func_decl", (67, 155), Some("Swift.Void"), Some("SwiftHelloWorld.Main.main()->()")),
      (
        "Main.swift",
        "function_conversion_expr",
        (46, 46),
        Some("(SwiftHelloWorld.Main.Type)->@Swift.MainActor()->()"),
        None
      ),
      ("Main.swift", "return_stmt", (46, 46), Some("Swift.Void"), None),
      ("Main.swift", "string_literal_expr", (147, 147), Some("Swift.String"), None),
      ("Main.swift", "struct_decl", (46, 158), Some("SwiftHelloWorld.Main"), Some("SwiftHelloWorld.Main")),
      ("Main.swift", "type_expr", (112, 112), Some("SwiftHelloWorldLib.HelloWorld"), None),
      ("Main.swift", "var_decl", (102, 102), Some("SwiftHelloWorldLib.HelloWorld"), None)
    )

    def apply(f: Path => Unit): Unit = {
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgTests") { tmpDir =>
        val packageSwift = tmpDir / "Package.swift"
        Files.writeString(packageSwift, PackageSwiftContent)

        val helloWorldSwift = tmpDir / "Sources" / "SwiftHelloWorldLib" / "HelloWorldSwift.swift"
        helloWorldSwift.createWithParentsIfNotExists(createParents = true)
        Files.writeString(helloWorldSwift, HelloWorldSwiftContent)

        val mainSwift = tmpDir / "Sources" / "SwiftHelloWorld" / "Main.swift"
        mainSwift.createWithParentsIfNotExists(createParents = true)
        Files.writeString(mainSwift, MainSwiftContent)

        val xCodeOut = tmpDir / "out.log"
        Files.writeString(xCodeOut, SwiftInvocation)

        f(tmpDir)
      }
    }
  }

  private def toResultList(
    mapping: SwiftTypeMapping
  ): List[(String, String, (Int, Int), Option[String], Option[String])] = {
    mapping
      .flatMap { case (filename, posToResolvedTypeInfo) =>
        posToResolvedTypeInfo.flatMap { case (range, typeInfos) =>
          typeInfos.map(typeInfo => (filename, typeInfo.nodeKind, range, typeInfo.typeFullname, typeInfo.declFullname))
        }
      }
      .toList
      .sortBy(t => s"${t._1}:${t._2}:${t._3._1}:${t._3._2}")
  }

  private def toResultList(
    mapping: MutableSwiftTypeMapping
  ): List[(String, String, (Int, Int), Option[String], Option[String])] = {
    val map = mapping.asScala.toMap.map { case (filename, mapping) =>
      filename -> mapping.asScala.toMap.map { case (range, set) =>
        range -> set.toSet
      }
    }
    toResultList(map)
  }

  private def toResultList(
    mapping: MutableSwiftTypeMapping,
    fileName: String
  ): List[(String, (Int, Int), Option[String], Option[String])] = {
    val map = mapping.asScala.toMap.map { case (filename, mapping) =>
      filename -> mapping.asScala.toMap.map { case (range, set) =>
        range -> set.toSet
      }
    }
    toResultList(map, fileName)
  }

  private def toResultList(
    mapping: SwiftTypeMapping,
    fileName: String
  ): List[(String, (Int, Int), Option[String], Option[String])] = {
    toResultList(mapping).filter(_._1.endsWith(fileName)).map(e => (e._2, e._3, e._4, e._5))
  }

  "Processing the compile output" should {

    "parsing the swiftc arguments correctly" in {
      val provider = SwiftTypesProvider.build(Config(), Seq(SwiftCompilerTestsFixture.SwiftInvocation))
      provider.parsedSwiftInvocations shouldBe SwiftCompilerTestsFixture.ExpectedSwiftInvocations
    }

    "filter external dependencies correctly" in SwiftCompilerTestsFixture { dir =>
      val provider = SwiftTypesProvider.build(
        Config().withInputPath(dir.toString).withOutputPath(dir.toString),
        Seq(SwiftCompilerTestsFixture.SwiftInvocation, SwiftCompilerTestsFixture.SwiftInvocationDependency)
      )
      provider.parsedSwiftInvocations shouldBe SwiftCompilerTestsFixture.ExpectedSwiftInvocations
    }

    "generate mapping from Json correctly" in {
      val mapping  = new MutableSwiftTypeMapping
      val provider = new SwiftTypesProvider(Config().withInputPath("."), Nil)
      provider.mappingFromJson(SwiftCompilerTestsFixture.JsonStringHelloWorldSwift, mapping)
      provider.mappingFromJson(SwiftCompilerTestsFixture.JsonStringMain, mapping)
      toResultList(mapping) shouldBe SwiftCompilerTestsFixture.ExpectedMapping
    }

    "generate mapping from Json with split correctly" in {
      val mapping  = new MutableSwiftTypeMapping
      val provider = new SwiftTypesProvider(Config().withInputPath("."), Nil)
      val input =
        s"${SwiftCompilerTestsFixture.JsonStringHelloWorldSwift}${SwiftCompilerTestsFixture.JsonStringMain}Error: some error foo"
      provider.mappingFromJson(input, mapping)
      toResultList(mapping) shouldBe SwiftCompilerTestsFixture.ExpectedMapping
    }

    "handle a SwiftPM project correctly" in SwiftCompilerTestsFixture { dir =>
      val config             = Config().withInputPath(dir.toString).withOutputPath(dir.toString).withSwiftBuild(true)
      val swiftTypesProvider = SwiftTypesProvider.apply(config)
      swiftTypesProvider match {
        case Some(provider) =>
          val Seq(swiftHelloWorldLibCommand, swiftHelloWorldCommand) = provider.parsedSwiftInvocations
          swiftHelloWorldLibCommand.exists(c => c.endsWith("/swiftc") || c.endsWith("\\swiftc.exe")) shouldBe true
          swiftHelloWorldLibCommand should contain("-module-name")
          swiftHelloWorldLibCommand should contain("SwiftHelloWorldLib")
          swiftHelloWorldLibCommand should contain("-parse-as-library")
          swiftHelloWorldLibCommand should contain("-c")
          swiftHelloWorldLibCommand should contain("-I")
          swiftHelloWorldLibCommand should contain("-target")
          swiftHelloWorldLibCommand should contain("-parse-as-library")
          swiftHelloWorldLibCommand should contain("-swift-version")
          swiftHelloWorldLibCommand should contain("-dump-ast")
          swiftHelloWorldLibCommand should contain("-dump-ast-format")
          swiftHelloWorldLibCommand should contain("json")

          swiftHelloWorldLibCommand.exists(c => c.endsWith("/swiftc") || c.endsWith("\\swiftc.exe")) shouldBe true
          swiftHelloWorldCommand should contain("-module-name")
          swiftHelloWorldCommand should contain("SwiftHelloWorld")
          swiftHelloWorldCommand should contain("-parse-as-library")
          swiftHelloWorldCommand should contain("-c")
          swiftHelloWorldCommand should contain("-I")
          swiftHelloWorldCommand should contain("-target")
          swiftHelloWorldCommand should contain("-parse-as-library")
          swiftHelloWorldCommand should contain("-swift-version")
          swiftHelloWorldCommand should contain("-dump-ast")
          swiftHelloWorldCommand should contain("-dump-ast-format")
          swiftHelloWorldCommand should contain("json")

          val allMappings        = provider.retrieveMappings()
          val helloWorldMappings = toResultList(allMappings, "HelloWorldSwift.swift")
          val mainMappings       = toResultList(allMappings, "Main.swift")

          mainMappings should contain(
            (
              "call_expr",
              (94, 106),
              Some("SwiftHelloWorldLib.HelloWorld"),
              Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
            )
          )
          mainMappings should contain(
            ("call_expr", (109, 134), Some("()"), Some("SwiftHelloWorldLib.HelloWorld.greet(from:Swift.String)->()"))
          )
          mainMappings should contain(
            (
              "constructor_ref_call_expr",
              (94, 94),
              Some("()->SwiftHelloWorldLib.HelloWorld"),
              Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
            )
          )
          mainMappings should contain oneElementOf List(
            ("func_decl", (49, 137), Some("Swift.Void"), Some("SwiftHelloWorld.Main.main()->()")),
            // TODO: in preparation for Swift 6.2.x migration, Void changed to () just like in the function signature
            ("func_decl", (49, 137), Some("()"), Some("SwiftHelloWorld.Main.main()->()"))
          )
          mainMappings should contain(("var_decl", (84, 84), Some("SwiftHelloWorldLib.HelloWorld"), None))

          helloWorldMappings should contain(
            (
              "accessor_decl",
              (87, 87),
              Some("Swift.String"),
              Some("SwiftHelloWorldLib.HelloWorld.suffix.getter:Swift.String")
            )
          )

          helloWorldMappings should contain(
            ("accessor_decl", (206, 291), Some("()"), Some("SwiftHelloWorldLib.HelloWorld.members.setter:Swift.Int"))
          )

          helloWorldMappings should contain(
            ("class_decl", (1, 478), Some("SwiftHelloWorldLib.HelloWorld"), Some("SwiftHelloWorldLib.HelloWorld"))
          )

          helloWorldMappings should contain oneElementOf List(
            // TODO: in preparation for Swift 6.2.x migration, modifiers are actually honored in constructor decl positions
            (
              "constructor_decl",
              (355, 386),
              Some("(SwiftHelloWorldLib.HelloWorld.Type)->()->SwiftHelloWorldLib.HelloWorld"),
              Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
            ),
            (
              "constructor_decl",
              (362, 386),
              Some("(SwiftHelloWorldLib.HelloWorld.Type)->()->SwiftHelloWorldLib.HelloWorld"),
              Some("SwiftHelloWorldLib.HelloWorld.init()->SwiftHelloWorldLib.HelloWorld")
            )
          )

          helloWorldMappings should contain oneElementOf List(
            // TODO: in preparation for Swift 6.2.x migration, modifiers are actually honored in func decl positions
            ("func_decl", (389, 475), Some("()"), Some("SwiftHelloWorldLib.HelloWorld.greet(from:Swift.String)->()")),
            ("func_decl", (396, 475), Some("()"), Some("SwiftHelloWorldLib.HelloWorld.greet(from:Swift.String)->()"))
          )
          helloWorldMappings should contain(
            (
              "member_ref_expr",
              (436, 436),
              Some("Swift.String"),
              Some("SwiftHelloWorldLib.HelloWorld.greeting:Swift.String")
            )
          )
          helloWorldMappings should contain(
            (
              "member_ref_expr",
              (465, 465),
              Some("Swift.String"),
              Some("SwiftHelloWorldLib.HelloWorld.suffix:Swift.String")
            )
          )
          helloWorldMappings should contain(
            ("var_decl", (41, 41), Some("Swift.String"), Some("SwiftHelloWorldLib.HelloWorld.greeting:Swift.String"))
          )
          helloWorldMappings should contain(
            ("var_decl", (87, 87), Some("Swift.String"), Some("SwiftHelloWorldLib.HelloWorld.suffix:Swift.String"))
          )
        case None =>
          fail("Can't build the SwiftTypesProvider")
      }
    }

    "handle a Xcode project correctly with a given compiler log file" in SwiftCompilerTestsFixture { dir =>
      val config = Config()
        .withInputPath(dir.toString)
        .withOutputPath(dir.toString)
        .withBuildLogPath(dir / "out.log")
        .withSwiftBuild(true)
      val swiftTypesProvider = SwiftTypesProvider.apply(config)
      swiftTypesProvider match {
        case Some(provider) =>
          provider.parsedSwiftInvocations shouldBe SwiftCompilerTestsFixture.ExpectedSwiftInvocations
        case None =>
          fail("Can't build the SwiftTypesProvider")
      }
    }

  }

}
