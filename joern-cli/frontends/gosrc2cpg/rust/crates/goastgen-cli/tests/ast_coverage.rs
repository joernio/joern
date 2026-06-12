use assert_cmd::Command;
use serde_json::Value;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::tempdir;

const INTENTIONALLY_UNSUPPORTED_BY_SCALA: &[(&str, &str)] = &[];

#[test]
fn emitted_node_types_are_known_to_scala_or_explicitly_inventoried() {
    let emitted = emitted_node_types_from_fixture_corpus();
    let supported = parser_ast_supported_node_types();
    let allowed_unsupported = INTENTIONALLY_UNSUPPORTED_BY_SCALA
        .iter()
        .map(|(node_type, _)| (*node_type).to_string())
        .collect::<BTreeSet<_>>();

    let unsupported = emitted
        .difference(&supported)
        .cloned()
        .collect::<BTreeSet<_>>();
    let unexpected = unsupported
        .difference(&allowed_unsupported)
        .cloned()
        .collect::<BTreeSet<_>>();
    assert!(
        unexpected.is_empty(),
        "Rust emitted node_type values not recognized by Scala ParserAst and not inventoried: {unexpected:?}\n\
         Add Scala handling, stop emitting them, or document them in INTENTIONALLY_UNSUPPORTED_BY_SCALA."
    );

    let missing_inventory_coverage = allowed_unsupported
        .difference(&emitted)
        .cloned()
        .collect::<BTreeSet<_>>();
    assert!(
        missing_inventory_coverage.is_empty(),
        "Unsupported Scala integration risks are not covered by the fixture corpus: {missing_inventory_coverage:?}\n\
         Add fixtures for these node types or remove obsolete inventory entries."
    );
}

fn emitted_node_types_from_fixture_corpus() -> BTreeSet<String> {
    let mut node_types = BTreeSet::new();
    for corpus_dir in immediate_child_dirs(&fixture_root()) {
        let tmp = tempdir().expect("creating temp dir");
        let out = tmp.path().join("out");
        Command::cargo_bin("goastgen")
            .unwrap()
            .args(["-out", out.to_str().unwrap(), corpus_dir.to_str().unwrap()])
            .assert()
            .success();

        let mut json_files = Vec::new();
        collect_json_files(&out, &mut json_files);
        assert!(
            !json_files.is_empty(),
            "fixture corpus {} produced no JSON files",
            corpus_dir.display()
        );
        for file in json_files {
            let value: Value =
                serde_json::from_slice(&fs::read(&file).expect("reading JSON output"))
                    .unwrap_or_else(|err| panic!("decoding {}: {err}", file.display()));
            collect_node_types(&value, &mut node_types);
        }
    }
    node_types
}

fn parser_ast_supported_node_types() -> BTreeSet<String> {
    let parser_ast = fs::read_to_string(parser_ast_path()).expect("reading ParserAst.scala");
    let mut supported = BTreeSet::new();
    let mut in_parser_ast = false;
    for line in parser_ast.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("object ParserAst") {
            in_parser_ast = true;
            continue;
        }
        if trimmed.starts_with("object ParserKeys") {
            break;
        }
        if !in_parser_ast {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("object ") {
            if let Some(name) = rest.split_whitespace().next() {
                if name != "NotHandledType" {
                    supported.insert(format!("ast.{name}"));
                }
            }
        }
    }
    supported
}

fn collect_node_types(value: &Value, out: &mut BTreeSet<String>) {
    match value {
        Value::Object(obj) => {
            if let Some(node_type) = obj.get("node_type").and_then(Value::as_str) {
                if node_type.starts_with("ast.") {
                    out.insert(node_type.to_string());
                }
            }
            for value in obj.values() {
                collect_node_types(value, out);
            }
        }
        Value::Array(values) => {
            for value in values {
                collect_node_types(value, out);
            }
        }
        _ => {}
    }
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/go-corpus")
}

fn parser_ast_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../src/main/scala/io/joern/gosrc2cpg/parser/ParserAst.scala")
}

fn immediate_child_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = fs::read_dir(root)
        .unwrap_or_else(|err| panic!("reading {}: {err}", root.display()))
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    dirs.sort();
    dirs
}

fn collect_json_files(root: &Path, out: &mut Vec<PathBuf>) {
    if !root.exists() {
        return;
    }
    for entry in
        fs::read_dir(root).unwrap_or_else(|err| panic!("reading {}: {err}", root.display()))
    {
        let entry = entry.expect("reading directory entry");
        let path = entry.path();
        if path.is_dir() {
            collect_json_files(&path, out);
        } else if path.extension().and_then(|value| value.to_str()) == Some("json") {
            out.push(path);
        }
    }
    out.sort();
}
