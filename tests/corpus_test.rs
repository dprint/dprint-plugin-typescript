//! Formats a corpus of real files and reports the files whose formatting keeps changing when
//! formatted again, which the specs don't catch because they're small. Formatting that changes
//! when formatted again, but stops changing after a few formats, is only counted.
//!
//! The corpus is the directories in the `DPRINT_TS_CORPUS` environment variable, separated
//! by the platform's path separator (ex. `;` on Windows). The test is skipped when it's not set.
//! In a git repository, only the files git tracks (including in submodules) are checked.
//!
//! ```sh
//! DPRINT_TS_CORPUS=../deno cargo test --release --test corpus -- --baseline tests/corpus_baselines/deno.txt
//! ```
//!
//! Files that fail to parse are skipped. The files with problems are the ones that are still
//! changing after `MAX_FORMATS` formats, whose formatted text doesn't parse, or that panic.
//!
//! - `--baseline <file>`: Only fails for the files that aren't listed in the file (one path per
//!   line, relative to its corpus directory, with `#` for comments), which allows fixing the
//!   existing ones over time.
//! - `--write-baseline <file>`: Writes the files with problems to the file.
//! - `--write-hashes <file>`: Writes a hash of each file's formatted text, which can be
//!   compared between two versions to find the files whose formatting changed.
//! - `--reduce <dir>`: Reduces each file that's still changing after `MAX_FORMATS` formats to a
//!   small snippet that still is, by removing statements and replacing expressions, and writes
//!   the distinct snippets to the directory along with a `summary.md` of how each changes.

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Mutex;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use dprint_plugin_typescript::FormatTextOptions;
use dprint_plugin_typescript::configuration::Configuration;
use dprint_plugin_typescript::configuration::ConfigurationBuilder;
use dprint_plugin_typescript::format_text;

const EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mts", "cts", "mjs", "cjs"];
/// The number of times a file is formatted to see if its formatting stops changing.
const MAX_FORMATS: usize = 20;
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

  if let Some(dir) = &args.reduce {
    let unstable_files = report
      .unsettled()
      .map(|unstable| files.iter().find(|file| file.name == unstable.name).unwrap())
      .collect::<Vec<_>>();
    reduce::reduce_files(&unstable_files, dir, &config);
  }

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
  reduce: Option<PathBuf>,
}

impl Args {
  fn parse() -> Self {
    let mut args = Args {
      baseline: None,
      write_baseline: None,
      write_hashes: None,
      reduce: None,
    };
    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
      match arg.as_str() {
        "--baseline" => args.baseline = Some(PathBuf::from(iter.next().expect("Expected a path after --baseline."))),
        "--write-baseline" => args.write_baseline = Some(PathBuf::from(iter.next().expect("Expected a path after --write-baseline."))),
        "--write-hashes" => args.write_hashes = Some(PathBuf::from(iter.next().expect("Expected a path after --write-hashes."))),
        "--reduce" => args.reduce = Some(PathBuf::from(iter.next().expect("Expected a path after --reduce."))),
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
  /// The number of formats after which the text stopped changing, or `None` when it
  /// didn't within `MAX_FORMATS`.
  settled_after: Option<usize>,
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
  fn unsettled(&self) -> impl Iterator<Item = &UnstableFile> {
    self.unstable.iter().filter(|file| file.settled_after.is_none())
  }

  fn problem_names(&self) -> BTreeSet<String> {
    let unsettled = self.unsettled().map(|file| &file.name);
    unsettled.chain(&self.invalid_output).chain(&self.panicked).cloned().collect()
  }

  fn print(&self) {
    eprintln!("Checked {} files and skipped {} that didn't parse.", self.checked, self.skipped);
    eprintln!("\nSlowest:");
    for (duration, name) in &self.slowest {
      eprintln!("  {:>8.1} ms  {}", duration.as_secs_f64() * 1000.0, name);
    }
    print_names("Panicked", &self.panicked);
    print_names("Formatted text didn't parse", &self.invalid_output);
    let settled_count = self.unstable.len() - self.unsettled().count();
    eprintln!(
      "\nChanged when formatted again, but stopped changing within {} formats: {}",
      MAX_FORMATS, settled_count
    );
    eprintln!("\nStill changing after {} formats ({}):", MAX_FORMATS, self.unsettled().count());
    for file in self.unsettled() {
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
      scope.spawn(|| {
        loop {
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
        }
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
  let Ok(settled_after) = get_settled_after(&file.path, second.clone(), config) else {
    return FileResult::InvalidOutput(hash);
  };
  let first_lines = first.lines().collect::<Vec<_>>();
  let second_lines = second.lines().collect::<Vec<_>>();
  let first_change = (0..first_lines.len().max(second_lines.len()))
    .find(|i| first_lines.get(*i) != second_lines.get(*i))
    .unwrap();
  let unstable_file = UnstableFile {
    name: file.name.clone(),
    settled_after,
    changed_lines: count_changed_lines(&first_lines, &second_lines),
    first_change: (
      first_change,
      first_lines.get(first_change).unwrap_or(&"").to_string(),
      second_lines.get(first_change).unwrap_or(&"").to_string(),
    ),
  };
  FileResult::Unstable(unstable_file, hash)
}

/// Formats the text from the second format until it stops changing, returning the number of
/// formats that took or `None` when it didn't stop within `MAX_FORMATS`. Errors when the
/// formatted text doesn't parse.
fn get_settled_after(path: &Path, second: String, config: &Configuration) -> Result<Option<usize>, ()> {
  let mut previous = second;
  for format_count in 3..=MAX_FORMATS {
    let next = format(path, previous.clone(), config).ok_or(())?;
    if next == previous {
      // the previous format was the last one that changed the text
      return Ok(Some(format_count - 1));
    }
    previous = next;
  }
  Ok(None)
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

/// Reduces the files that change when formatted again to small snippets that still do.
mod reduce {
  use std::collections::BTreeMap;
  use std::ops::Range;
  use std::path::Path;
  use std::sync::Arc;
  use std::sync::Mutex;
  use std::sync::atomic::AtomicUsize;
  use std::sync::atomic::Ordering;
  use std::time::Duration;
  use std::time::Instant;

  use dprint_plugin_typescript::configuration::Configuration;
  use dprint_plugin_typescript::dprint_swc_ext::common::SourceRanged;
  use dprint_plugin_typescript::dprint_swc_ext::common::StartSourcePos;
  use dprint_plugin_typescript::dprint_swc_ext::view::NodeKind;
  use dprint_plugin_typescript::dprint_swc_ext::view::NodeTrait;
  use dprint_plugin_typescript::dprint_swc_ext::view::ProgramInfoProvider;
  use dprint_plugin_typescript::dprint_swc_ext::view::with_ast_view;
  use dprint_plugin_typescript::parsing::ParseOptions;
  use dprint_plugin_typescript::parsing::parse_program;

  use super::CorpusFile;
  use super::format;
  use super::get_settled_after;

  /// The longest a file is reduced for, after which what it was reduced to so far is used.
  const MAX_DURATION_PER_FILE: Duration = Duration::from_secs(120);

  /// The kinds of nodes that are tried being replaced with an identifier.
  const EXPRESSION_KINDS: &[NodeKind] = &[
    NodeKind::ArrayLit,
    NodeKind::ArrowExpr,
    NodeKind::AssignExpr,
    NodeKind::AwaitExpr,
    NodeKind::BinExpr,
    NodeKind::CallExpr,
    NodeKind::ClassExpr,
    NodeKind::CondExpr,
    NodeKind::FnExpr,
    NodeKind::JSXElement,
    NodeKind::MemberExpr,
    NodeKind::NewExpr,
    NodeKind::Number,
    NodeKind::ObjectLit,
    NodeKind::OptChainExpr,
    NodeKind::ParenExpr,
    NodeKind::SeqExpr,
    NodeKind::Str,
    NodeKind::TaggedTpl,
    NodeKind::Tpl,
    NodeKind::TsAsExpr,
    NodeKind::UnaryExpr,
  ];

  pub fn reduce_files(files: &[&CorpusFile], dir: &Path, config: &Configuration) {
    std::fs::create_dir_all(dir).unwrap();
    // the reductions try a lot of text that panics, which would otherwise be printed
    std::panic::set_hook(Box::new(|_| {}));
    eprintln!("\nReducing {} files...", files.len());
    let next_index = AtomicUsize::new(0);
    let done_count = AtomicUsize::new(0);
    // the distinct snippets along with the files they came from
    let snippets = Mutex::new(BTreeMap::<(String, String), Vec<String>>::new());
    let thread_count = std::thread::available_parallelism().map(|count| count.get()).unwrap_or(4);
    std::thread::scope(|scope| {
      for _ in 0..thread_count {
        scope.spawn(|| {
          loop {
            let index = next_index.fetch_add(1, Ordering::Relaxed);
            let Some(file) = files.get(index) else {
              break;
            };
            let Ok(text) = std::fs::read_to_string(&file.path) else {
              continue;
            };
            let reduced = reduce(&file.path, text, config);
            let extension = file.path.extension().unwrap().to_string_lossy().to_string();
            snippets.lock().unwrap().entry((reduced, extension)).or_default().push(file.name.clone());
            let done_count = done_count.fetch_add(1, Ordering::Relaxed) + 1;
            eprintln!("  [{}/{}] {}", done_count, files.len(), file.name);
          }
        });
      }
    });

    // smallest first, since those are the easiest to look at
    let mut snippets = snippets.into_inner().unwrap().into_iter().collect::<Vec<_>>();
    snippets.sort_by_key(|((text, _), _)| text.len());
    let mut summary = String::from("# Reduced snippets that change when formatted again\n");
    for (i, ((text, extension), names)) in snippets.iter().enumerate() {
      let file_name = format!("{:03}.{}", i, extension);
      let path = dir.join(&file_name);
      std::fs::write(&path, text).unwrap();
      let first = format(&path, text.clone(), config).unwrap_or_default();
      let second = format(&path, first.clone(), config).unwrap_or_default();
      summary.push_str(&format!("\n## {}\n\nFrom: {}\n\n", file_name, names.join(", ")));
      for (title, text) in [("Input", text), ("First format", &first), ("Second format", &second)] {
        summary.push_str(&format!("{}:\n\n```{}\n{}\n```\n\n", title, extension, text.trim_end()));
      }
    }
    std::fs::write(dir.join("summary.md"), summary).unwrap();
    eprintln!("Wrote {} distinct snippets to {}.", snippets.len(), dir.display());
  }

  /// Reduces the text to a smaller text that still changes when formatted again by removing
  /// statements and replacing expressions with an identifier, largest first.
  fn reduce(path: &Path, text: String, config: &Configuration) -> String {
    let deadline = Instant::now() + MAX_DURATION_PER_FILE;
    let is_unstable = |text: &str| {
      std::panic::catch_unwind(|| {
        // only keep text without syntax errors, which swc would otherwise recover from
        let parsed_source = parse_program(ParseOptions {
          path,
          extension: None,
          text: Arc::from(text),
        });
        if !parsed_source.is_ok_and(|parsed_source| parsed_source.diagnostics().is_empty()) {
          return false;
        }
        let Some(first) = format(path, text.to_string(), config) else {
          return false;
        };
        let Some(second) = format(path, first.clone(), config) else {
          return false;
        };
        first != second && matches!(get_settled_after(path, second, config), Ok(None))
      })
      .unwrap_or(false)
    };
    let mut text = text;
    for replacement in ["", "x"] {
      // try the largest candidates first and, after a reduction, skip the larger candidates
      // since they were already tried, then start over until nothing more can be reduced
      loop {
        let mut reduced = false;
        let mut max_len = usize::MAX;
        loop {
          let Some(candidates) = get_candidates(path, &text, replacement.is_empty()) else {
            return text;
          };
          let found = candidates
            .into_iter()
            .filter(|range| range.len() <= max_len && range.len() > replacement.len())
            .find(|range| Instant::now() < deadline && is_unstable(&replace_range(&text, range, replacement)));
          match found {
            Some(range) => {
              max_len = range.len();
              text = replace_range(&text, &range, replacement);
              reduced = true;
            }
            None => break,
          }
        }
        if !reduced || Instant::now() > deadline {
          break;
        }
      }
    }
    text
  }

  fn replace_range(text: &str, range: &Range<usize>, replacement: &str) -> String {
    format!("{}{}{}", &text[..range.start], replacement, &text[range.end..])
  }

  /// Gets the ranges to try removing (statements, class members, and runs of them) or
  /// replacing (expressions), sorted from largest to smallest.
  fn get_candidates(path: &Path, text: &str, is_removing: bool) -> Option<Vec<Range<usize>>> {
    let parsed_source = parse_program(ParseOptions {
      path,
      extension: None,
      text: Arc::from(text),
    })
    .ok()?;
    let mut ranges = Vec::new();
    with_ast_view(parsed_source.program_info(), |program| {
      let mut pending = vec![program.as_node()];
      while let Some(node) = pending.pop() {
        let children = node.children();
        if is_removing {
          if matches!(
            node.kind(),
            NodeKind::Module | NodeKind::Script | NodeKind::BlockStmt | NodeKind::SwitchCase | NodeKind::Class
          ) {
            // the children and runs of them, which removes large parts of long lists quickly
            let mut run_len = children.len();
            while run_len >= 1 {
              for chunk in children.chunks(run_len) {
                ranges.push(get_range(&chunk[0]).start..get_range(chunk.last().unwrap()).end);
              }
              run_len /= 2;
            }
          } else if matches!(
            node.kind(),
            NodeKind::ObjectLit | NodeKind::ArrayLit | NodeKind::SeqExpr | NodeKind::CallExpr | NodeKind::NewExpr | NodeKind::VarDecl
          ) {
            // the items of comma separated lists, along with a comma
            for child in &children {
              ranges.push(with_comma(text, get_range(child)));
            }
          }
        } else if EXPRESSION_KINDS.contains(&node.kind()) {
          ranges.push(get_range(&node));
        }
        pending.extend(children);
      }
    });
    ranges.sort_by(|a, b| b.len().cmp(&a.len()).then(a.start.cmp(&b.start)));
    ranges.dedup();
    Some(ranges)
  }

  /// Extends the range of an item in a comma separated list to include the comma after it,
  /// or the comma before it when it's the last item, so that removing it leaves a valid list.
  fn with_comma(text: &str, range: Range<usize>) -> Range<usize> {
    let after = &text[range.end..];
    let after_trimmed = after.trim_start();
    if after_trimmed.starts_with(',') {
      return range.start..range.end + (after.len() - after_trimmed.len()) + 1;
    }
    let before = &text[..range.start];
    let before_trimmed = before.trim_end();
    if before_trimmed.ends_with(',') {
      return before_trimmed.len() - 1..range.end;
    }
    range
  }

  fn get_range(node: &impl SourceRanged) -> Range<usize> {
    node.start().as_byte_index(StartSourcePos::START_SOURCE_POS)..node.end().as_byte_index(StartSourcePos::START_SOURCE_POS)
  }
}
