use std::hint::black_box;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use dprint_plugin_typescript::configuration::BuiltinCategory;
use dprint_plugin_typescript::configuration::Configuration;
use dprint_plugin_typescript::configuration::ConfigurationBuilder;
use dprint_plugin_typescript::configuration::ImportGroup;
use dprint_plugin_typescript::configuration::ImportGroupMatch;
use dprint_plugin_typescript::configuration::ImportMatcher;
use dprint_plugin_typescript::configuration::SortOrder;
use dprint_plugin_typescript::format_parsed_source;
use dprint_plugin_typescript::format_text;
use dprint_plugin_typescript::FormatTextOptions;

fn main() {
  let iterations = env_usize("DPRINT_BENCH_ITERS").unwrap_or(10);
  let import_count = env_usize("DPRINT_BENCH_IMPORTS").unwrap_or(2_000);
  let named_count = env_usize("DPRINT_BENCH_NAMED_IMPORTS").unwrap_or(2_000);
  let parent_segments = env_usize("DPRINT_BENCH_PARENT_SEGMENTS").unwrap_or(6).max(1);
  let config = ConfigurationBuilder::new().build();
  let maintain_order_config = ConfigurationBuilder::new()
    .module_sort_import_declarations(SortOrder::Maintain)
    .module_sort_export_declarations(SortOrder::Maintain)
    .import_declaration_sort_named_imports(SortOrder::Maintain)
    .export_declaration_sort_named_exports(SortOrder::Maintain)
    .build();
  let grouped_imports_config = ConfigurationBuilder::new()
    .module_import_groups(vec![
      import_group(BuiltinCategory::Builtin),
      import_group(BuiltinCategory::External),
      import_group(BuiltinCategory::Parent),
      ImportGroup {
        matchers: ImportGroupMatch::Multiple(vec![
          ImportMatcher::Category(BuiltinCategory::Sibling),
          ImportMatcher::Category(BuiltinCategory::Index),
        ]),
      },
    ])
    .build();

  let sorted_imports_text = sorted_import_declarations(import_count, parent_segments);
  let sorted_exports_text = sorted_export_declarations(import_count, parent_segments);
  let grouped_imports_text = grouped_import_declarations(import_count, parent_segments);
  let sorted_named_imports_text = sorted_named_imports(named_count);
  let sorted_named_exports_text = sorted_named_exports(named_count);
  let sorted_imports_parsed = parse_source(&sorted_imports_text);
  let sorted_exports_parsed = parse_source(&sorted_exports_text);
  let grouped_imports_parsed = parse_source(&grouped_imports_text);
  let sorted_named_imports_parsed = parse_source(&sorted_named_imports_text);
  let sorted_named_exports_parsed = parse_source(&sorted_named_exports_text);

  println!("iterations: {iterations}");
  println!("import declarations: {import_count}");
  println!("named imports: {named_count}");
  println!("max parent segments: {parent_segments}");

  bench("sorted_import_declarations/full", iterations, || {
    run_format_text(&sorted_imports_text, &config)
  });
  bench("sorted_import_declarations/parsed", iterations, || {
    run_format_parsed_source(&sorted_imports_parsed, &config)
  });
  bench("maintain_import_declarations/parsed", iterations, || {
    run_format_parsed_source(&sorted_imports_parsed, &maintain_order_config)
  });
  bench("sorted_export_declarations/full", iterations, || {
    run_format_text(&sorted_exports_text, &config)
  });
  bench("sorted_export_declarations/parsed", iterations, || {
    run_format_parsed_source(&sorted_exports_parsed, &config)
  });
  bench("maintain_export_declarations/parsed", iterations, || {
    run_format_parsed_source(&sorted_exports_parsed, &maintain_order_config)
  });
  bench("grouped_import_declarations/full", iterations, || {
    run_format_text(&grouped_imports_text, &grouped_imports_config)
  });
  bench("grouped_import_declarations/parsed", iterations, || {
    run_format_parsed_source(&grouped_imports_parsed, &grouped_imports_config)
  });
  bench("sorted_named_imports/full", iterations, || {
    run_format_text(&sorted_named_imports_text, &config)
  });
  bench("sorted_named_imports/parsed", iterations, || {
    run_format_parsed_source(&sorted_named_imports_parsed, &config)
  });
  bench("maintain_named_imports/parsed", iterations, || {
    run_format_parsed_source(&sorted_named_imports_parsed, &maintain_order_config)
  });
  bench("sorted_named_exports/full", iterations, || {
    run_format_text(&sorted_named_exports_text, &config)
  });
  bench("sorted_named_exports/parsed", iterations, || {
    run_format_parsed_source(&sorted_named_exports_parsed, &config)
  });
  bench("maintain_named_exports/parsed", iterations, || {
    run_format_parsed_source(&sorted_named_exports_parsed, &maintain_order_config)
  });
}

fn bench(name: &str, iterations: usize, mut run: impl FnMut() -> usize) {
  for _ in 0..2 {
    black_box(run());
  }

  let mut timings = Vec::with_capacity(iterations);
  for _ in 0..iterations {
    let start = Instant::now();
    black_box(run());
    timings.push(start.elapsed());
  }

  timings.sort();
  let total = timings.iter().copied().fold(Duration::ZERO, |acc, timing| acc + timing);
  let min = timings[0];
  let median = timings[timings.len() / 2];
  let avg = total / timings.len() as u32;

  println!(
    "{name}: min={:.2}ms median={:.2}ms avg={:.2}ms",
    ms(min),
    ms(median),
    ms(avg),
  );
}

fn run_format_text(file_text: &str, config: &Configuration) -> usize {
  let result = format_text(FormatTextOptions {
    path: Path::new("bench.ts"),
    extension: None,
    text: file_text.to_string(),
    config,
    external_formatter: None,
  })
  .unwrap();
  result.as_deref().unwrap_or(file_text).len()
}

fn run_format_parsed_source(parsed_source: &ParsedSource, config: &Configuration) -> usize {
  let result = format_parsed_source(parsed_source, config, None).unwrap();
  result.as_deref().unwrap_or(parsed_source.text()).len()
}

fn parse_source(file_text: &str) -> ParsedSource {
  let media_type = MediaType::TypeScript;
  deno_ast::parse_program(deno_ast::ParseParams {
    specifier: ModuleSpecifier::parse("file:///bench.ts").unwrap(),
    capture_tokens: true,
    maybe_syntax: Some(deno_ast::get_syntax(media_type)),
    media_type,
    scope_analysis: false,
    text: Arc::from(file_text),
  })
  .unwrap()
}

fn sorted_import_declarations(import_count: usize, max_parent_segments: usize) -> String {
  let mut text = String::new();
  for i in (0..import_count).rev() {
    let parent_count = i % max_parent_segments;
    let prefix = if parent_count == 0 {
      ".".to_string()
    } else {
      "../".repeat(parent_count)
    };
    text.push_str(&format!(
      "import {{ Z{i}, a{i}, type T{i}, b{i} as B{i} }} from \"{prefix}pkg{}/mod{i}\";\n",
      i % 97,
    ));
  }
  text.push_str("\nexport const value = 1;\n");
  text
}

fn grouped_import_declarations(import_count: usize, max_parent_segments: usize) -> String {
  let mut text = String::new();
  for i in (0..import_count).rev() {
    let source = match i % 4 {
      0 => format!("node:bench{i}/mod"),
      1 => format!("pkg{}/mod{i}", i % 97),
      2 => format!("{}pkg{}/mod{i}", "../".repeat((i % max_parent_segments) + 1), i % 97),
      _ => format!("./pkg{}/mod{i}", i % 97),
    };
    text.push_str(&format!(
      "import {{ Z{i}, a{i}, type T{i}, b{i} as B{i} }} from \"{source}\";\n",
    ));
  }
  text.push_str("\nexport const value = 1;\n");
  text
}

fn sorted_export_declarations(export_count: usize, max_parent_segments: usize) -> String {
  let mut text = String::new();
  for i in (0..export_count).rev() {
    let parent_count = i % max_parent_segments;
    let prefix = if parent_count == 0 {
      ".".to_string()
    } else {
      "../".repeat(parent_count)
    };
    text.push_str(&format!(
      "export {{ Z{i}, a{i}, type T{i}, b{i} as B{i} }} from \"{prefix}pkg{}/mod{i}\";\n",
      i % 97,
    ));
  }
  text.push_str("\nexport const value = 1;\n");
  text
}

fn sorted_named_imports(named_count: usize) -> String {
  let mut text = String::from("import {\n");
  for i in (0..named_count).rev() {
    if i % 5 == 0 {
      text.push_str(&format!("  type T{i},\n"));
    } else {
      text.push_str(&format!("  Z{i} as z{i},\n"));
    }
  }
  text.push_str("} from \"pkg\";\n\nexport const value = 1;\n");
  text
}

fn sorted_named_exports(named_count: usize) -> String {
  let mut text = String::from("export {\n");
  for i in (0..named_count).rev() {
    if i % 5 == 0 {
      text.push_str(&format!("  type T{i},\n"));
    } else {
      text.push_str(&format!("  Z{i} as z{i},\n"));
    }
  }
  text.push_str("} from \"pkg\";\n\nexport const value = 1;\n");
  text
}

fn env_usize(name: &str) -> Option<usize> {
  std::env::var(name).ok().and_then(|value| value.parse().ok())
}

fn ms(duration: Duration) -> f64 {
  duration.as_secs_f64() * 1_000.0
}

fn import_group(category: BuiltinCategory) -> ImportGroup {
  ImportGroup {
    matchers: ImportGroupMatch::Single(ImportMatcher::Category(category)),
  }
}
