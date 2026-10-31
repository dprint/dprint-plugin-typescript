//! Formats a corpus of real files twice and reports the files whose formatting changes
//! when formatted again, which the specs don't catch because they're small.
//!
//! The corpus is the directories in the `DPRINT_TS_CORPUS` environment variable, separated
//! by the platform's path separator (ex. `;` on Windows). The test is skipped when it's not set.
//! In a git repository, only the files git tracks (including in submodules) are checked.
//!
//! ```sh
//! DPRINT_TS_CORPUS=../deno cargo test --release --test corpus -- --baseline tests/corpus_baselines/deno.txt
//! ```
//!
//! Files that fail to parse are skipped. Besides files that change when formatted again,
//! this reports the files whose formatted text doesn't parse and the files that panic.
//!
//! - `--baseline <file>`: Only fails for the files that aren't listed in the file (one path per
//!   line, relative to its corpus directory, with `#` for comments), which allows fixing the
//!   existing ones over time.
//! - `--write-baseline <file>`: Writes the files with problems to the file.
//! - `--write-hashes <file>`: Writes a hash of each file's formatted text, which can be
//!   compared between two versions to find the files whose formatting changed.

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
use std::time::Duration;
use std::time::Instant;

use dprint_plugin_typescript::configuration::Configuration;
use dprint_plugin_typescript::configuration::ConfigurationBuilder;
use dprint_plugin_typescript::format_text;
use dprint_plugin_typescript::FormatTextOptions;

const EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mts", "cts", "mjs", "cjs"];
const SKIPPED_DIR_NAMES: &[&str] = &["node_modules", ".git", "target"];

fn main() {
  let Some(corpus_dirs) = std::env::var_os("DPRINT_TS_CORPUS") else {
    eprintln!("Skipping the corpus test because DPRINT_TS_CORPUS isn't set.");
    return;
  };
  let args = Args::parse();
  let files = std::env::split_paths(&corpus_dirs).flat_map(|dir| get_corpus_files(&dir)).collect::<Vec<_>>();
  let config = ConfigurationBuilder::new().build();
  let report = check_files(&files, &config);
  report.print();

  if let Some(path) = &args.write_hashes {
    let text = report.hashes.iter().map(|(name, hash)| format!("{:016x} {}\n", hash, name)).collect::<String>();
    std::fs::write(path, text).unwrap();
    eprintln!("Wrote the hashes to {}.", path.display());
  }

  let problems = report.problem_names();
  if let Some(path) = &args.write_baseline {
    let text = problems.iter().map(|name| format!("{}\n", name)).collect::<String>();
    std::fs::write(path, text).unwrap();
    eprintln!("Wrote the baseline to {}.", path.display());
    return;
  }
  let baseline = match &args.baseline {
    Some(path) => std::fs::read_to_string(path)
      .unwrap()
      .lines()
      .map(|line| line.trim())
      .filter(|line| !line.is_empty() && !line.starts_with('#'))
      .map(|line| line.to_string())
      .collect::<BTreeSet<_>>(),
    None => BTreeSet::new(),
  };
  let fixed = baseline.difference(&problems).collect::<Vec<_>>();
  if !fixed.is_empty() {
    eprintln!("\nNo longer have problems, so these can be removed from the baseline:");
    for name in &fixed {
      eprintln!("  {}", name);
    }
  }
  let new_problems = problems.difference(&baseline).collect::<Vec<_>>();
  if !new_problems.is_empty() {
    eprintln!("\nHave problems and aren't in the baseline:");
    for name in &new_problems {
      eprintln!("  {}", name);
    }
    std::process::exit(1);
  }
}

struct Args {
  baseline: Option<PathBuf>,
  write_baseline: Option<PathBuf>,
  write_hashes: Option<PathBuf>,
}

impl Args {
  fn parse() -> Self {
    let mut args = Args {
      baseline: None,
      write_baseline: None,
      write_hashes: None,
    };
    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
      match arg.as_str() {
        "--baseline" => args.baseline = Some(PathBuf::from(iter.next().expect("Expected a path after --baseline."))),
        "--write-baseline" => args.write_baseline = Some(PathBuf::from(iter.next().expect("Expected a path after --write-baseline."))),
        "--write-hashes" => args.write_hashes = Some(PathBuf::from(iter.next().expect("Expected a path after --write-hashes."))),
        // ignore the arguments cargo passes to tests
        _ => {}
      }
    }
    args
  }
}

struct CorpusFile {
  /// Path relative to its corpus directory.
  name: String,
  path: PathBuf,
}

struct UnstableFile {
  name: String,
  /// The number of lines that differ between the first and second format.
  changed_lines: usize,
  /// The first line that differs, from the first and second format.
  first_change: (usize, String, String),
}

#[derive(Default)]
struct Report {
  checked: usize,
  skipped: usize,
  unstable: Vec<UnstableFile>,
  /// Files whose formatted text doesn't parse.
  invalid_output: Vec<String>,
  panicked: Vec<String>,
  slowest: Vec<(Duration, String)>,
  /// Hashes of the formatted text of the files that were checked.
  hashes: Vec<(String, u64)>,
}

impl Report {
  fn problem_names(&self) -> BTreeSet<String> {
    let unstable = self.unstable.iter().map(|file| &file.name);
    unstable.chain(&self.invalid_output).chain(&self.panicked).cloned().collect()
  }

  fn print(&self) {
    eprintln!("Checked {} files and skipped {} that didn't parse.", self.checked, self.skipped);
    eprintln!("\nSlowest:");
    for (duration, name) in &self.slowest {
      eprintln!("  {:>8.1} ms  {}", duration.as_secs_f64() * 1000.0, name);
    }
    print_names("Panicked", &self.panicked);
    print_names("Formatted text didn't parse", &self.invalid_output);
    eprintln!("\nChanged when formatted again ({}):", self.unstable.len());
    for file in &self.unstable {
      let (line, first, second) = &file.first_change;
      eprintln!("  {} ({} lines, first at line {})", file.name, file.changed_lines, line + 1);
      eprintln!("    first:  {}", first.trim_end());
      eprintln!("    second: {}", second.trim_end());
    }

    fn print_names(title: &str, names: &[String]) {
      if !names.is_empty() {
        eprintln!("\n{} ({}):", title, names.len());
        for name in names {
          eprintln!("  {}", name);
        }
      }
    }
  }
}

fn check_files(files: &[CorpusFile], config: &Configuration) -> Report {
  let next_index = AtomicUsize::new(0);
  let report = Mutex::new(Report::default());
  let thread_count = std::thread::available_parallelism().map(|count| count.get()).unwrap_or(4);
  std::thread::scope(|scope| {
    for _ in 0..thread_count {
      scope.spawn(|| loop {
        let index = next_index.fetch_add(1, Ordering::Relaxed);
        let Some(file) = files.get(index) else {
          break;
        };
        let start = Instant::now();
        let result = std::panic::catch_unwind(|| check_file(file, config));
        let elapsed = start.elapsed();
        let mut report = report.lock().unwrap();
        match result {
          Ok(FileResult::Skipped) => report.skipped += 1,
          Ok(FileResult::Stable(hash)) => {
            report.checked += 1;
            report.hashes.push((file.name.clone(), hash));
          }
          Ok(FileResult::Unstable(unstable_file, hash)) => {
            report.checked += 1;
            report.hashes.push((file.name.clone(), hash));
            report.unstable.push(unstable_file);
          }
          Ok(FileResult::InvalidOutput(hash)) => {
            report.checked += 1;
            report.hashes.push((file.name.clone(), hash));
            report.invalid_output.push(file.name.clone());
          }
          Err(_) => {
            report.checked += 1;
            report.panicked.push(file.name.clone());
          }
        }
        report.slowest.push((elapsed, file.name.clone()));
        report.slowest.sort_by(|a, b| b.0.cmp(&a.0));
        report.slowest.truncate(5);
      });
    }
  });
  let mut report = report.into_inner().unwrap();
  report.unstable.sort_by(|a, b| a.name.cmp(&b.name));
  report.invalid_output.sort();
  report.panicked.sort();
  report.hashes.sort();
  report
}

/// The result of checking a file, with the hash of its formatted text.
enum FileResult {
  Skipped,
  Stable(u64),
  Unstable(UnstableFile, u64),
  InvalidOutput(u64),
}

fn check_file(file: &CorpusFile, config: &Configuration) -> FileResult {
  let Ok(text) = std::fs::read_to_string(&file.path) else {
    return FileResult::Skipped;
  };
  let Some(first) = format(&file.path, text, config) else {
    return FileResult::Skipped;
  };
  let hash = hash_text(&first);
  let Some(second) = format(&file.path, first.clone(), config) else {
    return FileResult::InvalidOutput(hash);
  };
  if first == second {
    return FileResult::Stable(hash);
  }
  let first_lines = first.lines().collect::<Vec<_>>();
  let second_lines = second.lines().collect::<Vec<_>>();
  let first_change = (0..first_lines.len().max(second_lines.len()))
    .find(|i| first_lines.get(*i) != second_lines.get(*i))
    .unwrap();
  let unstable_file = UnstableFile {
    name: file.name.clone(),
    changed_lines: count_changed_lines(&first_lines, &second_lines),
    first_change: (
      first_change,
      first_lines.get(first_change).unwrap_or(&"").to_string(),
      second_lines.get(first_change).unwrap_or(&"").to_string(),
    ),
  };
  FileResult::Unstable(unstable_file, hash)
}

/// Formats the text, returning `None` when it doesn't parse.
fn format(path: &Path, text: String, config: &Configuration) -> Option<String> {
  let result = format_text(FormatTextOptions {
    path,
    extension: None,
    text: text.clone().into(),
    config,
    external_formatter: None,
    arena_pool: None,
  });
  match result {
    Ok(Some(formatted)) => Some(formatted),
    Ok(None) => Some(text),
    Err(_) => None,
  }
}

fn hash_text(text: &str) -> u64 {
  use std::hash::Hash;
  use std::hash::Hasher;

  let mut hasher = std::collections::hash_map::DefaultHasher::new();
  text.hash(&mut hasher);
  hasher.finish()
}

/// Counts the lines that aren't in the other text, which is enough to see how
/// much changed without diffing.
fn count_changed_lines(first: &[&str], second: &[&str]) -> usize {
  let mut counts = std::collections::HashMap::<&str, isize>::new();
  for line in first {
    *counts.entry(line).or_default() += 1;
  }
  for line in second {
    *counts.entry(line).or_default() -= 1;
  }
  counts.values().map(|count| count.unsigned_abs()).sum()
}

/// Gets the files to check in a corpus directory, which are the files git tracks
/// when it's a git repository so that local files don't change the results.
fn get_corpus_files(dir: &Path) -> Vec<CorpusFile> {
  let paths = get_git_tracked_files(dir).unwrap_or_else(|| {
    let mut paths = Vec::new();
    collect_files(dir, &mut paths);
    paths
  });
  paths
    .into_iter()
    .filter(|path| has_checked_extension(path))
    .map(|path| CorpusFile {
      name: path.strip_prefix(dir).unwrap().to_string_lossy().replace('\\', "/"),
      path,
    })
    .collect()
}

fn get_git_tracked_files(dir: &Path) -> Option<Vec<PathBuf>> {
  let output = std::process::Command::new("git")
    .args(["ls-files", "--recurse-submodules", "-z"])
    .current_dir(dir)
    .output()
    .ok()?;
  if !output.status.success() {
    return None;
  }
  let stdout = String::from_utf8(output.stdout).ok()?;
  let mut paths = stdout
    .split('\0')
    .filter(|name| !name.is_empty())
    .map(|name| dir.join(name))
    .collect::<Vec<_>>();
  paths.sort();
  Some(paths)
}

fn collect_files(dir: &Path, paths: &mut Vec<PathBuf>) {
  let Ok(entries) = std::fs::read_dir(dir) else {
    return;
  };
  let mut entries = entries.flatten().collect::<Vec<_>>();
  entries.sort_by_key(|entry| entry.file_name());
  for entry in entries {
    let Ok(file_type) = entry.file_type() else {
      continue;
    };
    if file_type.is_dir() {
      if !SKIPPED_DIR_NAMES.contains(&entry.file_name().to_string_lossy().as_ref()) {
        collect_files(&entry.path(), paths);
      }
    } else if file_type.is_file() {
      paths.push(entry.path());
    }
  }
}

fn has_checked_extension(path: &Path) -> bool {
  path.extension().is_some_and(|ext| EXTENSIONS.contains(&ext.to_string_lossy().as_ref()))
}
