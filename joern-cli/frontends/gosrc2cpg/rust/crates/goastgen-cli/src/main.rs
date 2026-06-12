use anyhow::{bail, Context, Result};
use clap::Parser;
use goastgen_core::{parse_file, write_json};
use ignore::WalkBuilder;
use regex::Regex;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(name = "goastgen", disable_version_flag = true)]
struct Args {
    #[arg(long = "version", action = clap::ArgAction::SetTrue)]
    version: bool,

    #[arg(short = 'o', long = "out", value_name = "OUT")]
    out: Option<PathBuf>,

    #[arg(long = "exclude", value_name = "REGEX")]
    exclude: Option<String>,

    #[arg(long = "include-packages", value_name = "REGEX")]
    include_packages: Option<String>,

    #[arg(value_name = "INPUT")]
    input: Option<PathBuf>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err:#}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let args = Args::parse_from(normalized_args());
    if args.version {
        println!("v{}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    let input = args.input.context("missing input path")?;
    let out = args.out.context("missing -out <dir>")?;
    let exclude = args.exclude.as_deref().map(Regex::new).transpose()?;
    let include_packages = IncludeFilter::new(args.include_packages.as_deref())?;

    let files = collect_inputs(&input, exclude.as_ref())?;
    for file in files {
        if !include_packages.matches(&input, &file) {
            continue;
        }

        let target = output_path(&input, &out, &file);
        match parse_file(&file).and_then(|value| write_json(&target, &value)) {
            Ok(()) => println!(
                "Converted AST for {} to {} ",
                file.display(),
                target.display()
            ),
            Err(err) => println!("{} {}", file.display(), err),
        }
    }
    Ok(())
}

fn normalized_args() -> Vec<String> {
    std::env::args()
        .map(|arg| match arg.as_str() {
            "-out" => "--out".into(),
            "-exclude" => "--exclude".into(),
            "-include-packages" => "--include-packages".into(),
            "-version" => "--version".into(),
            _ => arg,
        })
        .collect()
}

struct IncludeFilter {
    regex: Option<Regex>,
    package_suffixes: Vec<String>,
}

impl IncludeFilter {
    fn new(raw: Option<&str>) -> Result<Self> {
        let regex = raw.map(Regex::new).transpose()?;
        let package_suffixes = raw
            .map(|value| {
                value
                    .split(',')
                    .map(normalize_package_suffix)
                    .filter(|suffix| suffix.is_some())
                    .flatten()
                    .collect()
            })
            .unwrap_or_default();

        Ok(Self {
            regex,
            package_suffixes,
        })
    }

    fn matches(&self, input: &Path, file: &Path) -> bool {
        if self.regex.is_none() && self.package_suffixes.is_empty() {
            return true;
        }

        if file.file_name().and_then(|x| x.to_str()) == Some("go.mod") {
            return true;
        }

        let package_suffix = package_suffix(input, file);
        if self
            .package_suffixes
            .iter()
            .any(|suffix| suffix == &package_suffix)
        {
            return true;
        }

        self.regex
            .as_ref()
            .is_some_and(|re| re.is_match(&file.to_string_lossy()))
    }
}

fn normalize_package_suffix(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed == "." || trimmed == "/" {
        return Some(String::new());
    }
    if trimmed.is_empty() {
        return Some(String::new());
    }
    if trimmed.starts_with('/') {
        return Some(trimmed.trim_end_matches('/').to_string());
    }
    None
}

fn package_suffix(input: &Path, file: &Path) -> String {
    let base = if input.is_dir() {
        input
    } else {
        input.parent().unwrap_or(input)
    };
    let relative_parent = file
        .parent()
        .and_then(|parent| parent.strip_prefix(base).ok())
        .unwrap_or_else(|| Path::new(""));
    let parts: Vec<_> = relative_parent
        .components()
        .filter_map(|component| match component {
            std::path::Component::Normal(value) => value.to_str(),
            _ => None,
        })
        .collect();
    if parts.is_empty() {
        String::new()
    } else {
        format!("/{}", parts.join("/"))
    }
}

fn collect_inputs(input: &Path, exclude: Option<&Regex>) -> Result<Vec<PathBuf>> {
    if input.is_file() {
        if is_goastgen_input(input) && !is_excluded(input, exclude) {
            return Ok(vec![input.to_path_buf()]);
        }
        bail!(
            "input file is not a .go file or go.mod: {}",
            input.display()
        );
    }

    let mut files = Vec::new();
    for entry in WalkBuilder::new(input).hidden(false).build() {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && is_goastgen_input(path) && !is_excluded(path, exclude) {
            files.push(path.to_path_buf());
        }
    }
    files.sort();
    Ok(files)
}

fn is_goastgen_input(path: &Path) -> bool {
    path.file_name().and_then(|x| x.to_str()) == Some("go.mod")
        || path.extension().and_then(|x| x.to_str()) == Some("go")
}

fn is_excluded(path: &Path, exclude: Option<&Regex>) -> bool {
    exclude.is_some_and(|re| re.is_match(&path.to_string_lossy()))
}

fn output_path(input: &Path, out: &Path, file: &Path) -> PathBuf {
    let relative = if input.is_dir() {
        file.strip_prefix(input).unwrap_or(file)
    } else {
        file.file_name().map(Path::new).unwrap_or(file)
    };
    let mut target = out.join(relative);
    let file_name = target
        .file_name()
        .and_then(|x| x.to_str())
        .map(|x| format!("{x}.json"))
        .unwrap_or_else(|| "out.json".into());
    target.set_file_name(file_name);
    target
}
