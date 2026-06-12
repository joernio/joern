mod gomod;
mod mapper;

use anyhow::{Context, Result};
use serde_json::Value;
use std::fs;
use std::path::Path;

pub use gomod::parse_go_mod;
pub use mapper::parse_go_source;

pub fn parse_file(path: &Path) -> Result<Value> {
    let content =
        fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    match path.file_name().and_then(|x| x.to_str()) {
        Some("go.mod") => parse_go_mod(path, &content),
        _ => parse_go_source(path, &content),
    }
}

pub fn write_json(path: &Path, value: &Value) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("creating {}", parent.display()))?;
    }
    let bytes = serde_json::to_vec_pretty(value)?;
    fs::write(path, bytes).with_context(|| format!("writing {}", path.display()))
}
