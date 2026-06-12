use assert_cmd::Command;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::tempdir;

#[test]
fn fixture_corpus_output_is_deterministic_across_runs() {
    for corpus_dir in immediate_child_dirs(&fixture_root()) {
        let tmp = tempdir().expect("creating temp dir");
        let first_out = tmp.path().join("first");
        let second_out = tmp.path().join("second");

        run_goastgen(&corpus_dir, &first_out);
        run_goastgen(&corpus_dir, &second_out);

        let first = read_outputs(&first_out);
        let second = read_outputs(&second_out);
        assert_eq!(
            first,
            second,
            "non-deterministic output for fixture corpus {}",
            corpus_dir.display()
        );
    }
}

fn run_goastgen(input: &Path, out: &Path) {
    Command::cargo_bin("goastgen")
        .unwrap()
        .args(["-out", out.to_str().unwrap(), input.to_str().unwrap()])
        .assert()
        .success();
}

fn read_outputs(root: &Path) -> BTreeMap<String, Vec<u8>> {
    let mut files = Vec::new();
    collect_json_files(root, &mut files);
    assert!(
        !files.is_empty(),
        "goastgen produced no JSON output under {}",
        root.display()
    );

    let mut outputs = BTreeMap::new();
    for file in files {
        let relative = file
            .strip_prefix(root)
            .expect("output file under root")
            .to_string_lossy()
            .replace('\\', "/");
        let bytes =
            fs::read(&file).unwrap_or_else(|err| panic!("reading {}: {err}", file.display()));
        outputs.insert(relative, bytes);
    }
    outputs
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/go-corpus")
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
