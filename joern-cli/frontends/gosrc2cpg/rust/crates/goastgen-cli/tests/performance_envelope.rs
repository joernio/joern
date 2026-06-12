use assert_cmd::Command;
use std::env;
use std::fs;
use std::path::Path;
use std::time::{Duration, Instant};
use tempfile::tempdir;

const GENERATED_GO_FILES: usize = 160;
const DEFAULT_ENVELOPE_SECONDS: u64 = 45;

#[test]
fn generated_corpus_stays_inside_performance_envelope() {
    let tmp = tempdir().expect("creating temp dir");
    let src = tmp.path().join("src");
    let out = tmp.path().join("out");
    fs::create_dir(&src).expect("creating source dir");
    fs::write(src.join("go.mod"), "module example.com/perf\ngo 1.22\n").expect("writing go.mod");
    write_generated_corpus(&src);

    let start = Instant::now();
    let output = Command::cargo_bin("goastgen")
        .expect("finding goastgen binary")
        .args(["-out", out.to_str().unwrap(), src.to_str().unwrap()])
        .output()
        .expect("running goastgen");
    let elapsed = start.elapsed();

    assert!(
        output.status.success(),
        "goastgen failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        count_json_files(&out),
        GENERATED_GO_FILES + 1,
        "unexpected output file count"
    );

    let envelope = configured_envelope();
    assert!(
        elapsed <= envelope,
        "generated corpus took {elapsed:?}, expected at most {envelope:?}"
    );
}

fn write_generated_corpus(root: &Path) {
    for index in 0..GENERATED_GO_FILES {
        let package_dir = root.join(format!("pkg{index:03}"));
        fs::create_dir(&package_dir)
            .unwrap_or_else(|err| panic!("creating {}: {err}", package_dir.display()));
        fs::write(
            package_dir.join("file.go"),
            format!(
                r#"package pkg{index:03}

type Item{index} struct {{
  Name string
  Values []int
}}

func Make{index}(input []int) Item{index} {{
  values := make([]int, 0, len(input))
  for _, value := range input {{
    if value%2 == 0 {{
      values = append(values, value)
    }} else {{
      values = append(values, value+1)
    }}
  }}
  return Item{index}{{Name: "item", Values: values}}
}}

func Sum{index}(items []Item{index}) int {{
  total := 0
  for _, item := range items {{
    for _, value := range item.Values {{
      total += value
    }}
  }}
  return total
}}
"#
            ),
        )
        .unwrap_or_else(|err| panic!("writing generated file {index}: {err}"));
    }
}

fn configured_envelope() -> Duration {
    let seconds = env::var("GOASTGEN_PERF_ENVELOPE_SECONDS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(DEFAULT_ENVELOPE_SECONDS);
    Duration::from_secs(seconds)
}

fn count_json_files(root: &Path) -> usize {
    if !root.exists() {
        return 0;
    }

    let mut count = 0;
    for entry in
        fs::read_dir(root).unwrap_or_else(|err| panic!("reading {}: {err}", root.display()))
    {
        let entry = entry.expect("reading directory entry");
        let path = entry.path();
        if path.is_dir() {
            count += count_json_files(&path);
        } else if path.extension().and_then(|value| value.to_str()) == Some("json") {
            count += 1;
        }
    }
    count
}
