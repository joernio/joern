use anyhow::Result;
use serde_json::{json, Map, Value};
use std::path::Path;

pub fn parse_go_mod(path: &Path, content: &str) -> Result<Value> {
    let mut module = None;
    let mut dependencies = Vec::new();
    let mut in_require_block = false;

    for (line_index, raw_line) in content.lines().enumerate() {
        let line_number = line_index + 1;
        let line_without_comment = raw_line.split("//").next().unwrap_or("").trim();
        if line_without_comment.is_empty() {
            continue;
        }

        if let Some(rest) = line_without_comment.strip_prefix("module ") {
            let name = unquote_module_path(rest.trim()).to_string();
            module = Some(json!({
              "Name": name,
              "node_type": "mod.Module",
              "node_line_no": line_number,
              "node_col_no": first_non_ws_col(raw_line),
              "node_line_no_end": line_number,
              "node_col_no_end": legacy_end_col(raw_line, line_without_comment),
            }));
            continue;
        }

        if line_without_comment == "require (" {
            in_require_block = true;
            continue;
        }
        if in_require_block && line_without_comment == ")" {
            in_require_block = false;
            continue;
        }

        let require_line = if in_require_block {
            Some(line_without_comment)
        } else {
            line_without_comment.strip_prefix("require ").map(str::trim)
        };

        if let Some(require_line) = require_line {
            let parts: Vec<_> = require_line.split_whitespace().collect();
            if parts.len() >= 2 {
                dependencies.push(json!({
                  "Module": unquote_module_path(parts[0]),
                  "Version": parts[1],
                  "Indirect": raw_line.contains("// indirect"),
                  "node_type": "mod.Dependency",
                  "node_line_no": line_number,
                  "node_col_no": first_non_ws_col(raw_line),
                  "node_line_no_end": line_number,
                  "node_col_no_end": legacy_end_col(raw_line, line_without_comment),
                }));
            }
        }
    }

    let mut root = Map::new();
    root.insert(
        "node_filename".into(),
        Value::String(path.to_string_lossy().into_owned()),
    );
    if let Some(module) = module {
        root.insert("Module".into(), module);
    }
    root.insert("dependencies".into(), Value::Array(dependencies));
    Ok(Value::Object(root))
}

fn first_non_ws_col(line: &str) -> usize {
    line.chars()
        .position(|c| !c.is_whitespace())
        .map(|idx| idx + 1)
        .unwrap_or(1)
}

fn legacy_end_col(raw_line: &str, parsed_directive: &str) -> usize {
    first_non_ws_col(raw_line) + parsed_directive.len()
}

fn unquote_module_path(path: &str) -> &str {
    path.strip_prefix('"')
        .and_then(|value| value.strip_suffix('"'))
        .or_else(|| {
            path.strip_prefix('`')
                .and_then(|value| value.strip_suffix('`'))
        })
        .unwrap_or(path)
}
