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

func main() {
  ch := make(chan string)
  select {
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
        .stdout("v0.1.0\n");
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
fn variadic_parameters_emit_go_ast_ellipsis() {
    let go_json = render_single_file(
        r#"package main

func foo(argc int, argv ...*string) {}
"#,
    );

    let ellipsis = find_node_type(&go_json, "ast.Ellipsis").expect("missing ellipsis node");
    assert_eq!(ellipsis["Elt"]["node_type"], "ast.StarExpr");
}

#[test]
fn declaration_value_lists_are_flattened() {
    let go_json = render_single_file(
        r#"package main

var a, b = "first", "second"
var f, salary float32 = 10.0, 20.0
"#,
    );

    let specs = collect_nodes_of_type(&go_json, "ast.ValueSpec");
    assert_eq!(specs.len(), 2);
    assert_eq!(specs[0]["Names"].as_array().unwrap().len(), 2);
    assert_eq!(specs[0]["Values"].as_array().unwrap().len(), 2);
    assert_eq!(specs[1]["Names"].as_array().unwrap().len(), 2);
    assert_eq!(specs[1]["Values"].as_array().unwrap().len(), 2);
}

#[test]
fn implicit_length_arrays_emit_array_type() {
    let go_json = render_single_file(
        r#"package main

func main() {
  _ = [...]int{1, 2}
}
"#,
    );

    let array_type = find_node_type(&go_json, "ast.ArrayType").expect("missing array type");
    assert_eq!(array_type["Elt"]["Name"], "int");
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
    assert_eq!(star_expr["X"]["Name"], "int");

    let stars = collect_nodes_of_type(&go_json, "ast.StarExpr");
    assert!(stars.iter().any(|node| node["X"]["Name"] == "p"));
}

#[test]
fn selector_receivers_distinguish_variables_from_package_aliases() {
    let go_json = render_single_file(
        r#"package main

import "fmt"

type Person struct { fname string }

func main() {
  var a Person
  _ = a.fname
  fmt.Println(a.fname)
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
    assert_eq!(binary["X"]["Name"], "int64");
    assert_eq!(binary["Y"]["Name"], "float32");
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
}

#[test]
fn interface_methods_emit_field_func_types() {
    let go_json = render_single_file(
        r#"package main

type Speaker interface {
  Speak() string
}
"#,
    );

    let interface = find_node_type(&go_json, "ast.InterfaceType").expect("missing interface");
    let methods = interface["Methods"]["List"].as_array().unwrap();
    assert_eq!(methods.len(), 1);
    assert_eq!(methods[0]["Names"][0]["Name"], "Speak");
    assert_eq!(methods[0]["Type"]["node_type"], "ast.FuncType");
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
  }
}
"#,
    );

    let switch = find_node_type(&go_json, "ast.TypeSwitchStmt").expect("missing type switch");
    assert_eq!(switch["Assign"]["node_type"], "ast.AssignStmt");
    assert_eq!(switch["Assign"]["Lhs"][0]["Name"], "i");
    assert_eq!(
        switch["Assign"]["Rhs"][0]["node_type"],
        "ast.TypeAssertExpr"
    );
    assert!(switch["Assign"]["Rhs"][0]["Type"].is_null());
    assert_eq!(switch["Body"]["List"][0]["List"][0]["Name"], "int");
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
    assert_eq!(switch["Assign"]["node_type"], "ast.TypeAssertExpr");
    assert_eq!(switch["Assign"]["X"]["Name"], "x");
    assert!(switch["Assign"]["Type"].is_null());
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
