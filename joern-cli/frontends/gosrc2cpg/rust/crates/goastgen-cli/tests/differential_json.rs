use assert_cmd::Command;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command as StdCommand;
use tempfile::tempdir;

#[test]
fn rust_json_matches_reference_when_configured() {
    let Some(reference) = configured_reference_binary() else {
        eprintln!(
            "skipping differential JSON test; set GOASTGEN_REFERENCE or GOASTGEN_RUN_DIFFERENTIAL"
        );
        return;
    };
    assert!(
        reference.is_file(),
        "GOASTGEN_REFERENCE is not a file: {}",
        reference.display()
    );

    let corpus_root = fixture_root();
    let corpus_dirs = configured_corpus_dirs(&corpus_root);
    assert!(
        !corpus_dirs.is_empty(),
        "no fixture corpus directories under {}",
        corpus_root.display()
    );

    let mut failures = Vec::new();
    for corpus_dir in corpus_dirs {
        let corpus_dir = corpus_dir
            .canonicalize()
            .unwrap_or_else(|_| corpus_dir.to_path_buf());
        let tmp = tempdir().expect("creating temp dir");
        let reference_out = tmp.path().join("reference");
        let rust_out = tmp.path().join("rust");

        if let Err(err) = run_reference(&reference, &corpus_dir, &reference_out) {
            failures.push(format!(
                "{}: reference failed\n{err}",
                corpus_name(&corpus_dir)
            ));
            continue;
        }
        if let Err(err) = run_rust(&corpus_dir, &rust_out) {
            failures.push(format!("{}: rust failed\n{err}", corpus_name(&corpus_dir)));
            continue;
        }

        let reference_json = match read_json_tree(&reference_out, &corpus_dir) {
            Ok(value) => value,
            Err(err) => {
                failures.push(format!(
                    "{}: failed to read reference output\n{err}",
                    corpus_name(&corpus_dir)
                ));
                continue;
            }
        };
        let rust_json = match read_json_tree(&rust_out, &corpus_dir) {
            Ok(value) => value,
            Err(err) => {
                failures.push(format!(
                    "{}: failed to read rust output\n{err}",
                    corpus_name(&corpus_dir)
                ));
                continue;
            }
        };

        if let Some(diff) = format_json_diff(&corpus_name(&corpus_dir), &reference_json, &rust_json)
        {
            failures.push(diff);
        }
    }

    assert!(
        failures.is_empty(),
        "differential JSON mismatches:\n\n{}",
        failures.join("\n\n")
    );
}

fn configured_reference_binary() -> Option<PathBuf> {
    env::var_os("GOASTGEN_REFERENCE")
        .map(PathBuf::from)
        .or_else(|| env::var_os("GOASTGEN_RUN_DIFFERENTIAL").map(|_| default_reference_binary()))
}

fn default_reference_binary() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../bin/astgen")
        .join(host_reference_name())
}

fn host_reference_name() -> &'static str {
    #[cfg(target_os = "windows")]
    {
        "goastgen-windows.exe"
    }
    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    {
        "goastgen-linux-arm64"
    }
    #[cfg(all(target_os = "linux", not(target_arch = "aarch64")))]
    {
        "goastgen-linux"
    }
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    {
        "goastgen-macos-arm64"
    }
    #[cfg(all(target_os = "macos", not(target_arch = "aarch64")))]
    {
        "goastgen-macos"
    }
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/go-corpus")
}

fn configured_corpus_dirs(fixture_root: &Path) -> Vec<PathBuf> {
    let mut dirs = immediate_child_dirs(fixture_root);
    if let Some(real_corpus) = env::var_os("GOASTGEN_REAL_CORPUS") {
        let mut real_dirs = env::split_paths(&real_corpus)
            .inspect(|path| {
                assert!(
                    path.is_dir(),
                    "GOASTGEN_REAL_CORPUS entry is not a directory: {}",
                    path.display()
                );
            })
            .collect::<Vec<_>>();
        real_dirs.sort();
        dirs.extend(real_dirs);
    }
    dirs
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

fn run_reference(reference: &Path, input: &Path, out: &Path) -> Result<(), String> {
    let output = StdCommand::new(reference)
        .arg("-out")
        .arg(out)
        .arg(input)
        .output()
        .map_err(|err| err.to_string())?;
    check_output(output, "reference")
}

fn run_rust(input: &Path, out: &Path) -> Result<(), String> {
    let mut command = Command::cargo_bin("goastgen").map_err(|err| err.to_string())?;
    let output = command
        .arg("-out")
        .arg(out)
        .arg(input)
        .output()
        .map_err(|err| err.to_string())?;
    check_output(output, "rust")
}

fn check_output(output: std::process::Output, label: &str) -> Result<(), String> {
    if output.status.success() {
        Ok(())
    } else {
        Err(format!(
            "{label} command failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

fn read_json_tree(out: &Path, input_root: &Path) -> Result<BTreeMap<String, Value>, String> {
    let mut files = Vec::new();
    collect_json_files(out, &mut files)?;

    let mut values = BTreeMap::new();
    for file in files {
        let relative = file
            .strip_prefix(out)
            .map_err(|err| err.to_string())?
            .to_string_lossy()
            .replace('\\', "/");
        let bytes = fs::read(&file).map_err(|err| format!("reading {}: {err}", file.display()))?;
        let mut value: Value = serde_json::from_slice(&bytes)
            .map_err(|err| format!("decoding {}: {err}", file.display()))?;
        normalize_value(&mut value, input_root);
        values.insert(relative, value);
    }
    Ok(values)
}

fn collect_json_files(root: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if !root.exists() {
        return Ok(());
    }
    for entry in fs::read_dir(root).map_err(|err| format!("reading {}: {err}", root.display()))? {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        if path.is_dir() {
            collect_json_files(&path, out)?;
        } else if path.extension().and_then(|value| value.to_str()) == Some("json") {
            out.push(path);
        }
    }
    out.sort();
    Ok(())
}

fn normalize_value(value: &mut Value, input_root: &Path) {
    match value {
        Value::String(text) => {
            *text = normalize_string(text, input_root);
        }
        Value::Array(values) => {
            for value in values {
                normalize_value(value, input_root);
            }
        }
        Value::Object(values) => {
            for (key, value) in values.iter_mut() {
                if key == "node_id" || key == "node_reference_id" {
                    *value = Value::String("$ID".into());
                } else if key == "Obj" {
                    *value = Value::String("$OBJ".into());
                } else if key == "Scope" {
                    *value = Value::String("$SCOPE".into());
                } else if key == "Unresolved" {
                    *value = Value::String("$UNRESOLVED".into());
                } else {
                    normalize_value(value, input_root);
                }
            }
        }
        _ => {}
    }
}

fn normalize_string(text: &str, input_root: &Path) -> String {
    let root = input_root.to_string_lossy();
    let normalized_root = root.replace('\\', "/");
    text.replace(root.as_ref(), "$INPUT")
        .replace(&normalized_root, "$INPUT")
        .replace('\\', "/")
}

fn format_json_diff(
    corpus_name: &str,
    reference_json: &BTreeMap<String, Value>,
    rust_json: &BTreeMap<String, Value>,
) -> Option<String> {
    let reference_files = reference_json.keys().cloned().collect::<Vec<_>>();
    let rust_files = rust_json.keys().cloned().collect::<Vec<_>>();
    let mut message = format!("{corpus_name}: JSON output differs");
    if reference_files != rust_files {
        message.push_str(&format!(
            "\nreference files: {reference_files:?}\nrust files: {rust_files:?}"
        ));
        return Some(message);
    }

    for key in reference_files {
        match (reference_json.get(&key), rust_json.get(&key)) {
            (Some(reference_value), Some(rust_value)) if reference_value != rust_value => {
                if let Some(value_diff) = first_value_diff("$", reference_value, rust_value) {
                    message.push_str(&format!("\nfirst differing file: {key}\n{value_diff}"));
                    return Some(message);
                }
            }
            _ => {}
        }
    }
    None
}

fn first_value_diff(path: &str, reference: &Value, rust: &Value) -> Option<String> {
    match (reference, rust) {
        (Value::Object(reference_obj), Value::Object(rust_obj)) => {
            if reference_stub_matches_full_node(reference_obj, rust_obj)
                || reference_stub_matches_full_node(rust_obj, reference_obj)
            {
                return None;
            }
            let reference_keys = reference_obj.keys().cloned().collect::<BTreeSet<_>>();
            let rust_keys = rust_obj.keys().cloned().collect::<BTreeSet<_>>();
            if reference_keys != rust_keys {
                return Some(format!(
                    "{path}: object keys differ\nreference: {:?}\nrust: {:?}",
                    reference_keys, rust_keys
                ));
            }
            for key in reference_keys {
                if let Some(diff) = first_value_diff(
                    &format!("{path}.{key}"),
                    &reference_obj[&key],
                    &rust_obj[&key],
                ) {
                    return Some(diff);
                }
            }
            None
        }
        (Value::Array(reference_values), Value::Array(rust_values)) => {
            if reference_values.len() != rust_values.len() {
                return Some(format!(
                    "{path}: array length differs\nreference: {}\nrust: {}",
                    reference_values.len(),
                    rust_values.len()
                ));
            }
            for (index, (reference_value, rust_value)) in
                reference_values.iter().zip(rust_values.iter()).enumerate()
            {
                if let Some(diff) =
                    first_value_diff(&format!("{path}[{index}]"), reference_value, rust_value)
                {
                    return Some(diff);
                }
            }
            None
        }
        _ if reference == rust => None,
        _ => Some(format!(
            "{path}: value differs\nreference: {}\nrust: {}",
            short_json(reference),
            short_json(rust)
        )),
    }
}

fn reference_stub_matches_full_node(
    stub: &serde_json::Map<String, Value>,
    full: &serde_json::Map<String, Value>,
) -> bool {
    is_reference_stub(stub)
        && !is_reference_stub(full)
        && [
            "node_type",
            "node_line_no",
            "node_col_no",
            "node_line_no_end",
            "node_col_no_end",
        ]
        .iter()
        .all(|key| stub.get(*key) == full.get(*key))
}

fn is_reference_stub(obj: &serde_json::Map<String, Value>) -> bool {
    obj.contains_key("node_reference_id")
        && obj.keys().all(|key| {
            matches!(
                key.as_str(),
                "node_type"
                    | "node_id"
                    | "node_reference_id"
                    | "node_line_no"
                    | "node_col_no"
                    | "node_line_no_end"
                    | "node_col_no_end"
            )
        })
}

fn short_json(value: &Value) -> String {
    let text = serde_json::to_string(value).unwrap_or_else(|_| "<unprintable>".into());
    if text.len() > 500 {
        format!("{}...", &text[..500])
    } else {
        text
    }
}

fn corpus_name(path: &Path) -> String {
    path.file_name()
        .unwrap_or_else(|| std::ffi::OsStr::new("<unknown>"))
        .to_string_lossy()
        .into_owned()
}

#[test]
fn reference_stub_compares_equal_to_full_node_at_same_position() {
    let reference = serde_json::json!({
        "node_type": "ast.TypeSpec",
        "node_id": "$ID",
        "node_reference_id": "$ID",
        "node_line_no": 19,
        "node_col_no": 6,
        "node_line_no_end": 21,
        "node_col_no_end": 2
    });
    let rust = serde_json::json!({
        "node_type": "ast.TypeSpec",
        "node_id": "$ID",
        "node_line_no": 19,
        "node_col_no": 6,
        "node_line_no_end": 21,
        "node_col_no_end": 2,
        "Name": {"node_type": "ast.Ident", "Name": "Array"},
        "Type": {"node_type": "ast.StructType"},
        "Assign": 0
    });

    assert_eq!(first_value_diff("$", &reference, &rust), None);
    assert_eq!(first_value_diff("$", &rust, &reference), None);
}
