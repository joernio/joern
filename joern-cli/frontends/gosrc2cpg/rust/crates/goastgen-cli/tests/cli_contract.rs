use assert_cmd::Command;
use serde_json::Value;
use std::fs;
use tempfile::tempdir;

#[test]
fn accepts_legacy_flags_and_writes_go_and_mod_json() {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).unwrap();
    fs::write(
        src.join("go.mod"),
        "module example.com/demo\ngo 1.22\nrequire github.com/acme/lib v1.2.3\n",
    )
    .unwrap();
    fs::write(
        src.join("main.go"),
        r#"package main

import "fmt"

type Reader interface { Read(p []byte) (int, error) }
type Thing[T any] struct { path string }

func (r *Thing[T]) Read(p []byte) (int, error) {
  defer fmt.Println("x")
  go fmt.Println(<-make(chan string))
  return len(p), nil
}

func channels(in <-chan bool, out chan<- bool) {}

func main() {
  ch := make(chan string)
  select {
  case <-ch:
    fmt.Println("r")
  case ch <- "x":
    fallthrough
  default:
    fmt.Println("d")
  }
  _ = any(&Thing[int]{}).(*Thing[int])
}
"#,
    )
    .unwrap();

    let assert = Command::cargo_bin("goastgen")
        .unwrap()
        .args(["-out", out.to_str().unwrap(), src.to_str().unwrap()])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    assert!(stdout.contains("Converted AST for"));

    let go_json: Value =
        serde_json::from_slice(&fs::read(out.join("main.go.json")).unwrap()).unwrap();
    assert_eq!(go_json["node_type"], "ast.File");
    assert_eq!(go_json["Name"]["Name"], "main");
    assert!(contains_node_type(&go_json, "ast.DeferStmt"));
    assert!(contains_node_type(&go_json, "ast.GoStmt"));
    assert!(contains_node_type(&go_json, "ast.SelectStmt"));
    assert!(contains_node_type(&go_json, "ast.SendStmt"));
    assert!(contains_node_type(&go_json, "ast.TypeAssertExpr"));
    assert!(contains_node_type(&go_json, "ast.InterfaceType"));
    let chan_type = find_node_type(&go_json, "ast.ChanType").expect("missing channel type");
    assert!(chan_type["Begin"].as_u64().unwrap() > 0);
    assert_eq!(chan_type["Arrow"], 0);
    assert_eq!(chan_type["Dir"], 3);
    let chan_types = collect_nodes_of_type(&go_json, "ast.ChanType");
    let recv_chan = chan_types
        .iter()
        .find(|node| node["Dir"] == 2)
        .expect("missing receive-only channel");
    assert_eq!(recv_chan["Begin"], recv_chan["Arrow"]);
    let send_chan = chan_types
        .iter()
        .find(|node| node["Dir"] == 1)
        .expect("missing send-only channel");
    assert!(send_chan["Arrow"].as_u64().unwrap() > send_chan["Begin"].as_u64().unwrap());
    let send_stmt = find_node_type(&go_json, "ast.SendStmt").expect("missing send statement");
    assert!(send_stmt["Arrow"].as_u64().unwrap() > 0);

    let defer_stmt = find_node_type(&go_json, "ast.DeferStmt").expect("missing defer");
    assert!(defer_stmt["Defer"].as_u64().unwrap() > 0);
    let go_stmt = find_node_type(&go_json, "ast.GoStmt").expect("missing go statement");
    assert!(go_stmt["Go"].as_u64().unwrap() > 0);
    let type_assert =
        find_node_type(&go_json, "ast.TypeAssertExpr").expect("missing type assertion");
    assert!(type_assert["Lparen"].as_u64().unwrap() > 0);
    assert!(type_assert["Rparen"].as_u64().unwrap() > type_assert["Lparen"].as_u64().unwrap());
    let composites = collect_nodes_of_type(&go_json, "ast.CompositeLit");
    assert!(composites.iter().any(|node| node["Elts"].is_null()));
    let select_stmt = find_node_type(&go_json, "ast.SelectStmt").expect("missing select");
    assert!(select_stmt["Select"].as_u64().unwrap() > 0);
    assert!(
        select_stmt["Body"]["node_col_no"].as_u64().unwrap()
            > select_stmt["node_col_no"].as_u64().unwrap()
    );
    let comm_clause = find_node_type(&go_json, "ast.CommClause").expect("missing comm clause");
    assert!(comm_clause["Case"].as_u64().unwrap() > 0);
    assert!(comm_clause["Colon"].as_u64().unwrap() > comm_clause["Case"].as_u64().unwrap());
    assert_eq!(comm_clause["Body"].as_array().unwrap().len(), 1);
    assert_eq!(comm_clause["Comm"]["node_type"], "ast.ExprStmt");
    assert_eq!(comm_clause["Comm"]["X"]["node_type"], "ast.UnaryExpr");
    assert_eq!(comm_clause["Comm"]["X"]["Op"], "<-");
    let branch = find_node_type(&go_json, "ast.BranchStmt").expect("missing branch");
    assert_eq!(branch["Tok"], "fallthrough");
    assert!(branch["TokPos"].as_u64().unwrap() > 0);
    let comm_clauses = collect_nodes_of_type(&go_json, "ast.CommClause");
    let send_clause = comm_clauses
        .iter()
        .find(|clause| clause["Body"][0]["Tok"] == "fallthrough")
        .expect("missing send comm clause");
    assert_eq!(send_clause["node_line_no_end"], branch["node_line_no_end"]);
    assert_eq!(send_clause["node_col_no_end"], branch["node_col_no_end"]);
    let methods = collect_nodes_of_type(&go_json, "ast.FuncDecl");
    let read_method = methods
        .iter()
        .find(|node| node["Name"]["Name"] == "Read")
        .expect("missing method declaration");
    assert!(read_method["Name"].get("Obj").is_none());

    let mod_json: Value =
        serde_json::from_slice(&fs::read(out.join("go.mod.json")).unwrap()).unwrap();
    assert_eq!(mod_json["Module"]["Name"], "example.com/demo");
    assert_eq!(mod_json["dependencies"][0]["Module"], "github.com/acme/lib");
}

#[test]
fn version_supports_legacy_single_dash_form() {
    Command::cargo_bin("goastgen")
        .unwrap()
        .arg("-version")
        .assert()
        .success()
        .stdout("v0.2.0\n");
}

#[test]
fn exclude_suppresses_matching_files() {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("main.go"), "package main\nfunc main() {}\n").unwrap();

    Command::cargo_bin("goastgen")
        .unwrap()
        .args([
            "-exclude",
            "main\\.go",
            "-out",
            out.to_str().unwrap(),
            src.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert!(!out.join("main.go.json").exists());
}

#[test]
fn include_packages_accepts_legacy_comma_list_with_root_package() {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).unwrap();
    fs::create_dir(src.join("log")).unwrap();
    fs::create_dir(src.join("extra")).unwrap();
    fs::write(src.join("go.mod"), "module github.com/acme/lib\n").unwrap();
    fs::write(src.join("root.go"), "package lib\nfunc Root() {}\n").unwrap();
    fs::write(
        src.join("log").join("log.go"),
        "package log\nfunc Error() {}\n",
    )
    .unwrap();
    fs::write(
        src.join("extra").join("extra.go"),
        "package extra\nfunc Skip() {}\n",
    )
    .unwrap();

    Command::cargo_bin("goastgen")
        .unwrap()
        .args([
            "-include-packages",
            ",/log",
            "-out",
            out.to_str().unwrap(),
            src.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert!(out.join("go.mod.json").exists());
    assert!(out.join("root.go.json").exists());
    assert!(out.join("log").join("log.go.json").exists());
    assert!(!out.join("extra").join("extra.go.json").exists());
}

#[test]
fn go_work_is_ignored_and_vendor_sources_are_mirrored() {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir_all(src.join("vendor").join("example.com").join("lib")).unwrap();
    fs::write(src.join("go.mod"), "module example.com/root\n").unwrap();
    fs::write(src.join("go.work"), "go 1.22\n\nuse .\n").unwrap();
    fs::write(src.join("main.go"), "package main\n").unwrap();
    fs::write(
        src.join("vendor")
            .join("example.com")
            .join("lib")
            .join("lib.go"),
        "package lib\nfunc Name() string { return \"lib\" }\n",
    )
    .unwrap();

    Command::cargo_bin("goastgen")
        .unwrap()
        .args(["-out", out.to_str().unwrap(), src.to_str().unwrap()])
        .assert()
        .success();

    assert!(out.join("go.mod.json").exists());
    assert!(out.join("main.go.json").exists());
    assert!(!out.join("go.work.json").exists());
    assert!(out
        .join("vendor")
        .join("example.com")
        .join("lib")
        .join("lib.go.json")
        .exists());
}

#[test]
fn go_mod_dependency_positions_ignore_trailing_comments() {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).unwrap();
    fs::write(
        src.join("go.mod"),
        "module \"example.com/demo\"\n\nrequire (\n\tgithub.com/acme/lib v1.2.3\n\tgithub.com/acme/indirect v0.5.0 // indirect\n)\n",
    )
    .unwrap();

    Command::cargo_bin("goastgen")
        .unwrap()
        .args(["-out", out.to_str().unwrap(), src.to_str().unwrap()])
        .assert()
        .success();

    let mod_json: Value =
        serde_json::from_slice(&fs::read(out.join("go.mod.json")).unwrap()).unwrap();
    assert_eq!(mod_json["Module"]["Name"], "example.com/demo");
    let dependencies = mod_json["dependencies"].as_array().unwrap();
    assert_eq!(dependencies.len(), 2);
    assert_eq!(dependencies[1]["Module"], "github.com/acme/indirect");
    assert_eq!(dependencies[1]["Indirect"], true);
    assert_eq!(dependencies[1]["node_col_no"], 2);
    assert_eq!(dependencies[1]["node_col_no_end"], 33);
}

#[test]
fn variadic_parameters_and_calls_emit_go_ast_ellipsis() {
    let go_json = render_single_file(
        r#"package main

func foo(argc int, argv ...*string) {}
func format(argv ...*string) string { return "" }
func sink(value string) {}

func main() {
  args := []*string{}
  foo(1, args...)
  sink(format(args...))
}
"#,
    );

    let ellipsis = find_node_type(&go_json, "ast.Ellipsis").expect("missing ellipsis node");
    assert!(ellipsis["Ellipsis"].as_u64().unwrap() > 0);
    assert_eq!(ellipsis["node_col_no"], 25);
    assert_eq!(ellipsis["Elt"]["node_type"], "ast.StarExpr");

    let calls = collect_nodes_of_type(&go_json, "ast.CallExpr");
    let variadic_call = calls
        .iter()
        .find(|call| call["Fun"]["Name"] == "foo")
        .expect("missing variadic call");
    assert!(variadic_call["Ellipsis"].as_u64().unwrap() > 0);
    let outer_call = calls
        .iter()
        .find(|call| call["Fun"]["Name"] == "sink")
        .expect("missing outer call");
    assert_eq!(outer_call["Ellipsis"], 0);
}

#[test]
fn declaration_value_lists_are_flattened() {
    let go_json = render_single_file(
        r#"package main

var a, b = "first", "second"
var f, salary float32 = 10.0, 20.0
var levelNameHook LevelNameHook1
"#,
    );

    let specs = collect_nodes_of_type(&go_json, "ast.ValueSpec");
    assert_eq!(specs.len(), 3);
    assert_eq!(specs[0]["Names"].as_array().unwrap().len(), 2);
    assert!(specs[0].get("Type").is_none());
    assert_eq!(specs[0]["Values"].as_array().unwrap().len(), 2);
    assert_eq!(specs[1]["Names"].as_array().unwrap().len(), 2);
    assert_eq!(specs[1]["Values"].as_array().unwrap().len(), 2);
    assert_eq!(specs[2]["Names"][0]["Name"], "levelNameHook");
    assert!(specs[2]["Values"].is_null());

    let nested_var_json = render_single_file(
        r#"package main

var cases = []func(){
  func() { var nested int; _ = nested },
}
"#,
    );
    let top_var = collect_nodes_of_type(&nested_var_json, "ast.GenDecl")
        .into_iter()
        .find(|node| node["Tok"] == "var" && node["node_line_no"] == 3)
        .expect("missing top-level var declaration");
    assert_eq!(top_var["Specs"].as_array().unwrap().len(), 1);
}

#[test]
fn iota_emits_identifier_shape() {
    let go_json = render_single_file(
        r#"package main

const colorBlack = iota + 30
"#,
    );

    let binary = find_node_type(&go_json, "ast.BinaryExpr").expect("missing binary expr");
    assert_eq!(binary["X"]["node_type"], "ast.Ident");
    assert_eq!(binary["X"]["Name"], "iota");
}

#[test]
fn nested_binary_expressions_use_their_own_operator_position() {
    let go_json = render_single_file(
        r#"package main

var d = "x"
var _ = "a" + d + "b"
"#,
    );

    let binaries = collect_nodes_of_type(&go_json, "ast.BinaryExpr");
    let outer = binaries
        .iter()
        .find(|binary| binary["Y"]["Value"] == "\"b\"")
        .expect("missing outer binary expression");
    let inner = &outer["X"];
    assert!(outer["OpPos"].as_u64().unwrap() > inner["OpPos"].as_u64().unwrap());
}

#[test]
fn builtin_identifiers_stay_plain_unless_shadowed_locally() {
    let go_json = render_single_file(
        r#"package main

func main(s string) {
  _ = len(s)
  len := 1
  _ = len
}
"#,
    );

    let calls = collect_nodes_of_type(&go_json, "ast.CallExpr");
    let len_call = calls
        .iter()
        .find(|call| call["Fun"]["Name"] == "len")
        .expect("missing len call");
    assert!(len_call["Fun"].get("Obj").is_none());
    let identifiers = collect_nodes_of_type(&go_json, "ast.Ident");
    assert!(identifiers
        .iter()
        .any(|ident| ident["Name"] == "len" && ident.get("Obj").is_some()));
}

#[test]
fn future_local_declarations_do_not_bind_earlier_calls() {
    let go_json = render_single_file(
        r#"package main

func use(tag string) {
  tag = shortTag(tag)
  {
    shortTag := tag
    _ = shortTag
  }
}
"#,
    );

    let calls = collect_nodes_of_type(&go_json, "ast.CallExpr");
    let short_tag_call = calls
        .iter()
        .find(|call| call["Fun"]["Name"] == "shortTag")
        .expect("missing shortTag call");
    assert!(short_tag_call["Fun"].get("Obj").is_none());
    assert!(collect_nodes_of_type(&go_json, "ast.Ident")
        .iter()
        .any(|ident| ident["Name"] == "shortTag" && ident.get("Obj").is_some()));
}

#[test]
fn if_initializer_scope_does_not_leak_into_later_field_keys() {
    let go_json = render_single_file(
        r#"package main

type syncWriter struct{ lw any }

func wrap(w any) any {
  if lw, ok := w.(interface{}); ok {
    _ = lw
  }
  return syncWriter{lw: w}
}
"#,
    );

    let keyed_values = collect_nodes_of_type(&go_json, "ast.KeyValueExpr");
    let field_key = keyed_values
        .iter()
        .find(|node| node["Key"]["Name"] == "lw" && node["Value"]["Name"] == "w")
        .expect("missing syncWriter field key");
    assert!(field_key["Key"].get("Obj").is_none());
    assert!(collect_nodes_of_type(&go_json, "ast.Ident")
        .iter()
        .any(|ident| ident["Name"] == "lw" && ident.get("Obj").is_some()));
}

#[test]
fn empty_communication_clause_body_is_null() {
    let go_json = render_single_file(
        r#"package main

func main() {
  ch := make(chan int)
  select {
  case ch <- 1:
  }
}
"#,
    );

    let clause = find_node_type(&go_json, "ast.CommClause").expect("missing comm clause");
    assert_eq!(clause["Comm"]["node_type"], "ast.SendStmt");
    assert!(clause["Body"].is_null());
}

#[test]
fn named_results_and_local_types_are_in_scope() {
    let go_json = render_single_file(
        r#"package main

func makeS() (n int, err error) {
  type S struct{ ID int }
  s := S{ID: n}
  err = nil
  return s.ID, err
}
"#,
    );

    let composites = collect_nodes_of_type(&go_json, "ast.CompositeLit");
    let local_type_lit = composites
        .iter()
        .find(|node| node["Type"]["Name"] == "S")
        .expect("missing local type literal");
    assert!(local_type_lit["Type"].get("Obj").is_some());

    let assignments = collect_nodes_of_type(&go_json, "ast.AssignStmt");
    let err_assignment = assignments
        .iter()
        .find(|node| node["Tok"] == "=" && node["Lhs"][0]["Name"] == "err")
        .expect("missing named-result assignment");
    assert!(err_assignment["Lhs"][0].get("Obj").is_some());
}

#[test]
fn assignments_emit_legacy_token_position() {
    let go_json = render_single_file(
        r#"package main

func main() {
  value := 1
  fileInfo, _ := stat()
}

func stat() (int, error) { return 0, nil }
"#,
    );

    let assignments = collect_nodes_of_type(&go_json, "ast.AssignStmt");
    let assignment = assignments
        .iter()
        .find(|assignment| assignment["Lhs"][0]["Name"] == "value")
        .expect("missing assignment");
    assert_eq!(assignment["Tok"], ":=");
    assert!(assignment["TokPos"].as_u64().unwrap() > 0);
    let multi_assignment = assignments
        .iter()
        .find(|assignment| assignment["Lhs"][0]["Name"] == "fileInfo")
        .expect("missing multi assignment");
    assert!(multi_assignment["Lhs"][0]["Obj"].is_object());
    assert!(multi_assignment["Lhs"][1]["Obj"].is_object());
}

#[test]
fn range_statements_emit_legacy_positions() {
    let go_json = render_single_file(
        r#"package main

func main() {
  for key, value := range []int{1} {
    _, _ = key, value
  }
}
"#,
    );

    let range = find_node_type(&go_json, "ast.RangeStmt").expect("missing range");
    assert!(range["For"].as_u64().unwrap() > 0);
    assert!(range["Range"].as_u64().unwrap() > 0);
    assert!(range["TokPos"].as_u64().unwrap() > 0);

    let no_key_range_json = render_single_file(
        r#"package main

func main() {
  for range servers {
  }
}
"#,
    );
    let no_key_range =
        find_node_type(&no_key_range_json, "ast.RangeStmt").expect("missing no-key range");
    assert_eq!(no_key_range["Tok"], "ILLEGAL");
    assert_eq!(no_key_range["TokPos"], 0);
    assert_eq!(no_key_range["X"]["Name"], "servers");
    assert!(no_key_range["X"].get("node_reference_id").is_none());
}

#[test]
fn implicit_length_arrays_emit_array_type() {
    let go_json = render_single_file(
        r#"package main

type Pair struct { Key string; w int }
type Diode interface{}
type Holder struct { Diode }

func main(w int) {
  _ = Pair{Key: "x", w: w}
  _ = Holder{Diode: nil}
  _ = [...]int{1, 2}
  _ = [2]int{1, 2}
  _ = []int{
    // ignored comment
    1,
    // ignored comment
    2,
  }
}
"#,
    );

    let array_types = collect_nodes_of_type(&go_json, "ast.ArrayType");
    let implicit_array = array_types
        .iter()
        .find(|node| node.get("Len").is_none())
        .expect("missing implicit array type");
    assert!(implicit_array["Lbrack"].as_u64().unwrap() > 0);
    assert_eq!(implicit_array["Elt"]["Name"], "int");
    let explicit_array = array_types
        .iter()
        .find(|node| node["Len"]["Value"] == "2")
        .expect("missing explicit array type");
    assert!(explicit_array["Lbrack"].as_u64().unwrap() > 0);
    let commented_literal = collect_nodes_of_type(&go_json, "ast.CompositeLit")
        .into_iter()
        .find(|node| {
            node["Type"]["node_type"] == "ast.ArrayType"
                && node["Type"].get("Len").is_none()
                && node["Elts"].as_array().is_some_and(|elts| elts.len() == 2)
                && node["Elts"][0]["Value"] == "1"
                && node["Elts"][1]["Value"] == "2"
        })
        .expect("missing comment-filtered array literal");
    assert_eq!(commented_literal["Elts"].as_array().unwrap().len(), 2);
    let composite = find_node_type(&go_json, "ast.CompositeLit").expect("missing composite lit");
    assert_eq!(composite["Incomplete"], false);
    assert!(composite["Lbrace"].as_u64().unwrap() > 0);
    assert!(composite["Rbrace"].as_u64().unwrap() > composite["Lbrace"].as_u64().unwrap());
    let key_value = find_node_type(&go_json, "ast.KeyValueExpr").expect("missing keyed element");
    assert!(key_value["Colon"].as_u64().unwrap() > 0);
    assert!(key_value["Key"].get("Obj").is_none());
    let keyed_values = collect_nodes_of_type(&go_json, "ast.KeyValueExpr");
    let local_key = keyed_values
        .iter()
        .find(|node| node["Key"]["Name"] == "w")
        .expect("missing local-name field key");
    assert!(local_key["Key"].get("Obj").is_some());
    let embedded_key = keyed_values
        .iter()
        .find(|node| node["Key"]["Name"] == "Diode")
        .expect("missing embedded field key");
    assert!(embedded_key["Key"].get("Obj").is_some());

    let package_key_json = render_single_file(
        r#"package main

type Decoder struct{ stringMapType int }
var stringMapType = 1

func main() {
  _ = Decoder{stringMapType: stringMapType}
}
"#,
    );
    let package_key = collect_nodes_of_type(&package_key_json, "ast.KeyValueExpr")
        .into_iter()
        .find(|node| node["Key"]["Name"] == "stringMapType")
        .expect("missing package-declared field key");
    assert!(package_key["Key"].get("Obj").is_some());
}

#[test]
fn map_types_emit_legacy_map_position() {
    let go_json = render_single_file(
        r#"package main

func main() {
  key := "x"
  _ = map[string]int{}
  _ = map[string]int{key: 1}
}
"#,
    );

    let map_type = find_node_type(&go_json, "ast.MapType").expect("missing map type");
    assert!(map_type["Map"].as_u64().unwrap() > 0);
    let keyed_values = collect_nodes_of_type(&go_json, "ast.KeyValueExpr");
    let key_expr = keyed_values
        .iter()
        .find(|node| node["Key"]["Name"] == "key")
        .expect("missing map key expression");
    assert!(key_expr["Key"].get("Obj").is_some());
}

#[test]
fn package_and_struct_emit_legacy_fields() {
    let go_json = render_single_file(
        r#"// Package docs mention package before the actual declaration.

package main

type Thing struct {
  Name string
}

// trailing package mention must not extend ast.File
"#,
    );

    assert!(go_json["Package"].as_u64().unwrap() > 1);
    assert_eq!(go_json["Package"], 65);
    let struct_type = find_node_type(&go_json, "ast.StructType").expect("missing struct");
    assert!(struct_type["Struct"].as_u64().unwrap() > 0);
    assert_eq!(struct_type["Incomplete"], false);
    assert_eq!(go_json["node_line_no_end"], struct_type["node_line_no_end"]);
    assert_eq!(go_json["node_col_no_end"], struct_type["node_col_no_end"]);
}

#[test]
fn dereference_expression_emits_star_expr() {
    let go_json = render_single_file(
        r#"package main

func main() {
  var p *int
  _ = *p
}
"#,
    );

    let star_expr = find_node_type(&go_json, "ast.StarExpr").expect("missing star expression");
    assert!(star_expr["Star"].as_u64().unwrap() > 0);
    assert_eq!(star_expr["X"]["Name"], "int");

    let stars = collect_nodes_of_type(&go_json, "ast.StarExpr");
    assert!(stars.iter().any(|node| node["X"]["Name"] == "p"));
}

#[test]
fn slice_expressions_emit_legacy_shape() {
    let go_json = render_single_file(
        r#"package main

func main() {
  values := []int{1, 2}
  _ = values[:0]
  _ = values[0:1:2]
}
"#,
    );

    let slices = collect_nodes_of_type(&go_json, "ast.SliceExpr");
    assert_eq!(slices.len(), 2);

    assert!(slices[0].get("Low").is_none());
    assert_eq!(slices[0]["High"]["Value"], "0");
    assert_eq!(slices[0]["Slice3"], false);

    assert_eq!(slices[1]["Low"]["Value"], "0");
    assert_eq!(slices[1]["High"]["Value"], "1");
    assert_eq!(slices[1]["Max"]["Value"], "2");
    assert_eq!(slices[1]["Slice3"], true);
}

#[test]
fn type_conversions_emit_call_expr_shape() {
    let go_json = render_single_file(
        r#"package main

func main() {
  _ = []byte("b")
}
"#,
    );

    let calls = collect_nodes_of_type(&go_json, "ast.CallExpr");
    let conversion = calls
        .iter()
        .find(|call| call["Fun"]["node_type"] == "ast.ArrayType")
        .expect("missing slice conversion call");
    assert!(conversion["Lparen"].as_u64().unwrap() > 0);
    assert_eq!(conversion["Ellipsis"], 0);
    assert_eq!(conversion["Args"][0]["Value"], "\"b\"");
}

#[test]
fn dereferenced_pointer_conversions_emit_legacy_star_shape() {
    let go_json = render_single_file(
        r#"package main

func main(d interface{}) {
  _ = *(*[]byte)(d)
}
"#,
    );

    let assignment = find_node_type(&go_json, "ast.AssignStmt").expect("missing assignment");
    let deref = &assignment["Rhs"][0];
    assert_eq!(deref["node_type"], "ast.StarExpr");
    assert_eq!(deref["X"]["node_type"], "ast.CallExpr");
    assert_eq!(deref["X"]["Fun"]["node_type"], "ast.ParenExpr");
    assert!(deref["X"]["Fun"]["Lparen"].as_u64().unwrap() > deref["Star"].as_u64().unwrap());
    assert!(
        deref["X"]["Fun"]["Rparen"].as_u64().unwrap()
            > deref["X"]["Fun"]["Lparen"].as_u64().unwrap()
    );
    assert!(deref["X"]["Lparen"].as_u64().unwrap() > deref["Star"].as_u64().unwrap());
}

#[test]
fn index_expression_brackets_follow_the_index_operand() {
    let go_json = render_single_file(
        r#"package main

func main(p *int) {
  _ = (*[2]int)(p)[1]
}
"#,
    );

    let index = find_node_type(&go_json, "ast.IndexExpr").expect("missing index expression");
    assert!(index["Lbrack"].as_u64().unwrap() > index["X"]["Rparen"].as_u64().unwrap());
    assert!(index["Rbrack"].as_u64().unwrap() > index["Lbrack"].as_u64().unwrap());
}

#[test]
fn selector_receivers_distinguish_variables_from_package_aliases() {
    let go_json = render_single_file(
        r#"package main

import (
  "fmt"
  "log"
)

type Person struct { fname string }
type Logger struct { field int }

func main() {
  var a Person
  _ = a.fname
  fmt.Println(a.fname)
}

func localLog() {
  log := Logger{}
  _ = log.field
}

func packageLog() {
  log.SetOutput(nil)
}
"#,
    );

    let selectors = collect_nodes_of_type(&go_json, "ast.SelectorExpr");
    assert!(selectors
        .iter()
        .any(|node| node["X"]["Name"] == "a" && node["X"].get("Obj").is_some()));
    assert!(selectors
        .iter()
        .any(|node| node["X"]["Name"] == "fmt" && node["X"].get("Obj").is_none()));
    assert!(selectors
        .iter()
        .any(|node| node["X"]["Name"] == "log" && node["X"].get("Obj").is_some()));
    assert!(selectors
        .iter()
        .any(|node| node["Sel"]["Name"] == "SetOutput"
            && node["X"]["Name"] == "log"
            && node["X"].get("Obj").is_none()));
}

#[test]
fn generic_union_constraints_emit_binary_expression() {
    let go_json = render_single_file(
        r#"package main

func foo[T int64 | float32](value T) {}
"#,
    );

    let binary = find_node_type(&go_json, "ast.BinaryExpr").expect("missing generic union");
    assert_eq!(binary["Op"], "|");
    assert!(binary["OpPos"].as_u64().unwrap() > 0);
    assert_eq!(binary["X"]["Name"], "int64");
    assert_eq!(binary["Y"]["Name"], "float32");

    let generic_index_json =
        render_single_file("package main\ntype Box[T any] []T\ntype IntBox Box[int]\n");
    let index =
        find_node_type(&generic_index_json, "ast.IndexExpr").expect("missing generic index");
    assert!(index["Lbrack"].as_u64().unwrap() > 0);
    assert!(index["Rbrack"].as_u64().unwrap() > index["Lbrack"].as_u64().unwrap());

    let generic_index_list_json = render_single_file(
        "package main\ntype Pair[K any, V any] struct{}\ntype StringInt Pair[string, int]\n",
    );
    let index_list = find_node_type(&generic_index_list_json, "ast.IndexListExpr")
        .expect("missing generic index list");
    assert_eq!(index_list["Indices"].as_array().unwrap().len(), 2);
    assert!(index_list["Rbrack"].as_u64().unwrap() > index_list["Lbrack"].as_u64().unwrap());
}

#[test]
fn unnamed_function_type_parameters_keep_null_names() {
    let go_json = render_single_file(
        r#"package main

type Operation func(int, int) int
"#,
    );

    let fields = collect_nodes_of_type(&go_json, "ast.Field");
    assert!(fields
        .iter()
        .any(|node| node["Type"]["Name"] == "int" && node["Names"].is_null()));
}

#[test]
fn logical_not_preserves_operator_token() {
    let go_json = render_single_file(
        r#"package main

func main() {
  _ = !true
}
"#,
    );

    let unary = find_node_type(&go_json, "ast.UnaryExpr").expect("missing unary expression");
    assert_eq!(unary["Op"], "!");
    assert!(unary["OpPos"].as_u64().unwrap() > 0);

    let commented_paren = render_single_file(
        r#"package main

func ok(x int) bool {
  return (
    // ignored comment
    x == 1)
}
"#,
    );
    let paren = find_node_type(&commented_paren, "ast.ParenExpr").expect("missing paren expr");
    assert_eq!(paren["X"]["node_type"], "ast.BinaryExpr");
}

#[test]
fn interface_methods_emit_field_func_types() {
    let go_json = render_single_file(
        r#"package main

type Speaker interface {
  Speak() string
  UnmarshalYAML(unmarshal func(interface{}) error) error
}
"#,
    );

    let interface = find_node_type(&go_json, "ast.InterfaceType").expect("missing interface");
    assert!(interface["Interface"].as_u64().unwrap() > 0);
    assert_eq!(interface["Incomplete"], false);
    let methods = interface["Methods"]["List"].as_array().unwrap();
    assert_eq!(methods.len(), 2);
    assert_eq!(methods[0]["Names"][0]["Name"], "Speak");
    assert_eq!(methods[0]["Type"]["node_type"], "ast.FuncType");
    assert_eq!(methods[0]["Type"]["Func"], 0);
    assert_eq!(methods[1]["Type"]["Func"], 0);
    assert!(
        methods[1]["Type"]["Params"]["List"][0]["Type"]["Func"]
            .as_u64()
            .unwrap()
            > 0
    );
    assert!(interface["Methods"]["Opening"].as_u64().unwrap() > 0);
    assert!(
        interface["Methods"]["Closing"].as_u64().unwrap()
            > interface["Methods"]["Opening"].as_u64().unwrap()
    );
    assert_eq!(
        methods[0]["Type"]["Results"]["List"][0]["Type"]["Name"],
        "string"
    );
}

#[test]
fn type_switches_emit_type_assert_assign_shape() {
    let go_json = render_single_file(
        r#"package main

func method() {
  var x interface{}
  switch i := x.(type) {
  case int:
    println(i)
  }
}
"#,
    );

    let switch = find_node_type(&go_json, "ast.TypeSwitchStmt").expect("missing type switch");
    assert!(switch["Switch"].as_u64().unwrap() > 0);
    assert_eq!(switch["Assign"]["node_type"], "ast.AssignStmt");
    assert_eq!(switch["Assign"]["Lhs"][0]["Name"], "i");
    assert!(switch["Assign"]["Lhs"][0]["Obj"].is_object());
    assert!(switch["Assign"]["TokPos"].as_u64().unwrap() > 0);
    assert_eq!(
        switch["Assign"]["Rhs"][0]["node_type"],
        "ast.TypeAssertExpr"
    );
    assert!(switch["Assign"]["Rhs"][0].get("Type").is_none());
    assert!(switch["Assign"]["Rhs"][0]["Lparen"].as_u64().unwrap() > 0);
    assert!(
        switch["Assign"]["Rhs"][0]["Rparen"].as_u64().unwrap()
            > switch["Assign"]["Rhs"][0]["Lparen"].as_u64().unwrap()
    );
    let case_clause = &switch["Body"]["List"][0];
    assert!(case_clause["Case"].as_u64().unwrap() > 0);
    assert!(case_clause["Colon"].as_u64().unwrap() > case_clause["Case"].as_u64().unwrap());
    assert_eq!(case_clause["List"][0]["Name"], "int");
    assert!(case_clause["Body"][0]["X"]["Args"][0]["Obj"].is_object());
}

#[test]
fn type_switches_without_alias_emit_type_assert_expr() {
    let go_json = render_single_file(
        r#"package main

func method() {
  var x interface{}
  switch x.(type) {
  case nil:
  }
}
"#,
    );

    let switch = find_node_type(&go_json, "ast.TypeSwitchStmt").expect("missing type switch");
    assert!(switch["Switch"].as_u64().unwrap() > 0);
    assert_eq!(switch["Assign"]["node_type"], "ast.TypeAssertExpr");
    assert_eq!(switch["Assign"]["X"]["Name"], "x");
    assert!(switch["Assign"].get("Type").is_none());
}

#[test]
fn switches_emit_legacy_switch_position() {
    let go_json = render_single_file(
        r#"package main

func method(x int) {
  switch x {
  case 1:
  default:
    // case appears in this comment after the colon
    return
  }
}
"#,
    );

    let switch = find_node_type(&go_json, "ast.SwitchStmt").expect("missing switch");
    assert!(switch["Switch"].as_u64().unwrap() > 0);
    assert!(switch["Body"]["List"][0]["Body"].is_null());
    let default_clause = &switch["Body"]["List"][1];
    assert!(default_clause["Case"].as_u64().unwrap() < default_clause["Colon"].as_u64().unwrap());
}

#[test]
fn for_statements_emit_legacy_for_position() {
    let go_json = render_single_file(
        r#"package main

func method() {
  for i := 0; i < 1; i++ {
  }
}
"#,
    );

    let for_stmt = find_node_type(&go_json, "ast.ForStmt").expect("missing for statement");
    assert!(for_stmt["For"].as_u64().unwrap() > 0);
    assert_eq!(for_stmt["Post"]["node_type"], "ast.IncDecStmt");
    assert!(for_stmt["Post"]["TokPos"].as_u64().unwrap() > 0);

    let labeled_json = render_single_file(
        r#"package main

func method() {
label:
  for {
    break label
  }
}
"#,
    );
    let label = find_node_type(&labeled_json, "ast.LabeledStmt").expect("missing labeled stmt");
    assert!(label["Colon"].as_u64().unwrap() > label["Label"]["NamePos"].as_u64().unwrap());
    assert!(label["Label"].get("Obj").is_some());
    let branch = find_node_type(&labeled_json, "ast.BranchStmt").expect("missing branch stmt");
    assert_eq!(branch["Label"]["Name"], "label");
    assert!(branch["Label"].get("Obj").is_some());
}

#[test]
fn file_comments_match_legacy_null_when_no_comments_are_emitted() {
    let go_json = render_single_file(
        r#"package main

func main() {}
"#,
    );

    assert!(go_json["Comments"].is_null());

    let versioned = render_single_file(
        r#"//go:build go1.7
// +build go1.7

package main
"#,
    );
    assert_eq!(versioned["GoVersion"], "go1.7");

    let old_build_tag = render_single_file(
        r#"// +build go1.12

package main
"#,
    );
    assert_eq!(old_build_tag["GoVersion"], "");

    let no_decls = render_single_file(
        r#"// Package main has no declarations.
package main
"#,
    );
    assert!(no_decls["Decls"].is_null());
}

#[test]
fn only_file_root_emits_node_filename_for_go_source_json() {
    let go_json = render_single_file(
        r#"package main

func main() {}
"#,
    );

    assert!(go_json.get("node_filename").is_some());
    let funcs = collect_nodes_of_type(&go_json, "ast.FuncDecl");
    assert!(!funcs.is_empty());
    assert!(funcs.iter().all(|node| node.get("node_filename").is_none()));
}

#[test]
fn imports_and_blocks_emit_legacy_position_fields() {
    let go_json = render_single_file(
        r#"package main

import "fmt"

func init() {}

func helper() {}

func main() {
  helper()
  fmt.Println("x")
}
"#,
    );

    let imports = go_json["Imports"].as_array().unwrap();
    assert_eq!(imports.len(), 1);
    assert!(imports[0].get("Path").is_none());
    assert!(imports[0].get("node_reference_id").is_some());

    let import = find_node_type(&go_json, "ast.ImportSpec").expect("missing import");
    assert_eq!(import["EndPos"], 0);
    assert_eq!(import["Path"]["ValuePos"], 22);

    let functions = collect_nodes_of_type(&go_json, "ast.FuncDecl");
    let function = functions
        .iter()
        .find(|function| function["Name"]["Name"] == "helper")
        .expect("missing function");
    assert_eq!(function["Name"]["Obj"].as_object().unwrap().len(), 0);
    let init_function = functions
        .iter()
        .find(|function| function["Name"]["Name"] == "init")
        .expect("missing init function");
    assert!(init_function["Name"].get("Obj").is_none());
    assert!(function["Type"]["Func"].as_u64().unwrap() > 0);
    assert!(function["Type"]["Params"]["Opening"].as_u64().unwrap() > 0);
    assert!(
        function["Type"]["Params"]["Closing"].as_u64().unwrap()
            > function["Type"]["Params"]["Opening"].as_u64().unwrap()
    );
    assert!(function["Type"]["Params"]["List"].is_null());

    let block = find_node_type(&go_json, "ast.BlockStmt").expect("missing block");
    assert!(block["Lbrace"].as_u64().unwrap() > 0);
    assert!(block["Rbrace"].as_u64().unwrap() > block["Lbrace"].as_u64().unwrap());

    let call = find_node_type(&go_json, "ast.CallExpr").expect("missing call");
    assert!(call["Lparen"].as_u64().unwrap() > 0);
    assert!(call["Rparen"].as_u64().unwrap() > call["Lparen"].as_u64().unwrap());
    assert_eq!(call["Ellipsis"], 0);
    let calls = collect_nodes_of_type(&go_json, "ast.CallExpr");
    let helper_call = calls
        .iter()
        .find(|node| node["Fun"]["Name"] == "helper")
        .expect("missing helper call");
    assert!(helper_call["Fun"].get("Obj").is_some());
    assert!(helper_call["Args"].is_null());

    let return_json = render_single_file(
        r#"package main

func main() int {
  return 1
}
"#,
    );
    let return_stmt = find_node_type(&return_json, "ast.ReturnStmt").expect("missing return");
    assert!(return_stmt["Return"].as_u64().unwrap() > 0);
    assert_eq!(return_stmt["Results"].as_array().unwrap().len(), 1);

    let bare_return_json = render_single_file(
        r#"package main

func main() {
  return
}
"#,
    );
    let bare_return = find_node_type(&bare_return_json, "ast.ReturnStmt").expect("missing return");
    assert!(bare_return["Results"].is_null());

    let if_json = render_single_file(
        r#"package main

func main() {
  if true {
    return
  }
}
"#,
    );
    let if_stmt = find_node_type(&if_json, "ast.IfStmt").expect("missing if");
    assert!(if_stmt["If"].as_u64().unwrap() > 0);

    let if_init_json = render_single_file(
        r#"package main

func main() {
  if value := 1; value > 0 {
    return
  }
}
"#,
    );
    let if_with_init = find_node_type(&if_init_json, "ast.IfStmt").expect("missing if");
    assert_eq!(if_with_init["Init"]["node_type"], "ast.AssignStmt");
    assert_eq!(if_with_init["Init"]["Tok"], ":=");
    assert!(if_with_init["Init"]["TokPos"].as_u64().unwrap() > 0);

    let no_imports = render_single_file(
        r#"package main

func main() {}
"#,
    );
    assert!(no_imports["Imports"].is_null());
    let empty_block = find_node_type(&no_imports, "ast.BlockStmt").expect("missing block");
    assert!(empty_block["List"].is_null());

    let grouped_imports = render_single_file(
        r#"package main

import (
  "fmt"
  "os"
)

func main() {
  _, _ = fmt.Println, os.Args
}
"#,
    );
    let gen_decl =
        find_node_type(&grouped_imports, "ast.GenDecl").expect("missing grouped import decl");
    assert_eq!(gen_decl["Tok"], "import");
    assert!(gen_decl["Lparen"].as_u64().unwrap() > 0);
    assert!(gen_decl["Rparen"].as_u64().unwrap() > gen_decl["Lparen"].as_u64().unwrap());
}

#[test]
fn malformed_go_files_are_reported_without_json_output() {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("bad.go"), "package main\nfunc main( {\n").unwrap();

    let assert = Command::cargo_bin("goastgen")
        .unwrap()
        .args(["-out", out.to_str().unwrap(), src.to_str().unwrap()])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    assert!(stdout.contains("bad.go"));
    assert!(stdout.contains("parse error"));
    assert!(!out.join("bad.go.json").exists());
}

#[test]
fn recoverable_tree_sitter_errors_do_not_skip_legacy_accepted_sources() {
    let go_json = render_single_file(
        r#"
package main

func foo() Node {
   var boo = int64(0)
   return Node{
     value: boo,
   }
}

type Node struct {
   value string
}
"#,
    );

    assert!(find_node_type(&go_json, "ast.FuncDecl").is_some());
    assert!(find_node_type(&go_json, "ast.TypeSpec").is_some());
}

fn render_single_file(source: &str) -> Value {
    let tmp = tempdir().unwrap();
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("main.go"), source).unwrap();

    Command::cargo_bin("goastgen")
        .unwrap()
        .args(["-out", out.to_str().unwrap(), src.to_str().unwrap()])
        .assert()
        .success();

    serde_json::from_slice(&fs::read(out.join("main.go.json")).unwrap()).unwrap()
}

fn find_node_type<'a>(value: &'a Value, node_type: &str) -> Option<&'a Value> {
    match value {
        Value::Object(obj) => {
            if obj.get("node_type").and_then(Value::as_str) == Some(node_type) {
                Some(value)
            } else {
                obj.values()
                    .find_map(|value| find_node_type(value, node_type))
            }
        }
        Value::Array(values) => values
            .iter()
            .find_map(|value| find_node_type(value, node_type)),
        _ => None,
    }
}

fn collect_nodes_of_type<'a>(value: &'a Value, node_type: &str) -> Vec<&'a Value> {
    let mut out = Vec::new();
    collect_nodes_of_type_inner(value, node_type, &mut out);
    out
}

fn collect_nodes_of_type_inner<'a>(value: &'a Value, node_type: &str, out: &mut Vec<&'a Value>) {
    match value {
        Value::Object(obj) => {
            if obj.get("node_type").and_then(Value::as_str) == Some(node_type) {
                out.push(value);
            }
            for value in obj.values() {
                collect_nodes_of_type_inner(value, node_type, out);
            }
        }
        Value::Array(values) => {
            for value in values {
                collect_nodes_of_type_inner(value, node_type, out);
            }
        }
        _ => {}
    }
}

fn contains_node_type(value: &Value, node_type: &str) -> bool {
    match value {
        Value::Object(obj) => {
            obj.get("node_type").and_then(Value::as_str) == Some(node_type)
                || obj
                    .values()
                    .any(|value| contains_node_type(value, node_type))
        }
        Value::Array(values) => values
            .iter()
            .any(|value| contains_node_type(value, node_type)),
        _ => false,
    }
}
