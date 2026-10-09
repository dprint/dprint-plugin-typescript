//! Sorts Tailwind CSS class lists.
//!
//! This module is self-contained so that it can be extracted into its own crate.
//!
//! The handling of whitespace, duplicates, unknown classes, and partially sorted
//! class lists is patterned off prettier-plugin-tailwindcss and its tests, which
//! is MIT licensed (see `LICENSE.prettier-plugin-tailwindcss`). Unlike that plugin,
//! this doesn't load Tailwind or a project's configuration. The class order comes
//! from the static tables in `tables.rs`.

mod tables;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::sync::OnceLock;

use rustc_hash::FxHashMap;

use tables::*;

/// Options for sorting a piece of text containing class names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SortOptions {
  /// Keeps the first class where it is. Use this when the text follows something
  /// it is not separated from by whitespace, as the first class is then only part
  /// of a class name.
  pub ignore_first: bool,
  /// Keeps the last class where it is. Use this when the text precedes something
  /// it is not separated from by whitespace.
  pub ignore_last: bool,
  /// Removes the leading whitespace instead of collapsing it to a single space.
  pub collapse_start: bool,
  /// Removes the trailing whitespace instead of collapsing it to a single space.
  pub collapse_end: bool,
}

impl Default for SortOptions {
  fn default() -> Self {
    Self {
      ignore_first: false,
      ignore_last: false,
      collapse_start: true,
      collapse_end: true,
    }
  }
}

/// Sorts the class names in the provided text in the order Tailwind CSS recommends.
///
/// Unknown classes go first in their original order, whitespace is collapsed, and
/// duplicate Tailwind classes are removed. Borrows the text when it doesn't change.
pub fn sort_class_names<'a>(text: &'a str, options: &SortOptions) -> Cow<'a, str> {
  // matches what Prettier does for class attributes containing a template
  if text.is_empty() || text.contains("{{") {
    return Cow::Borrowed(text);
  }
  let mut class_names = text.split(is_class_separator).filter(|class_name| !class_name.is_empty());
  let Some(first) = class_names.next() else {
    return if text == " " { Cow::Borrowed(text) } else { Cow::Borrowed(" ") };
  };
  let (prefix, first) = if options.ignore_first { (Some(first), None) } else { (None, Some(first)) };
  let suffix = if options.ignore_last { class_names.next_back() } else { None };
  let class_names = first.into_iter().chain(class_names);
  let has_leading_space = !options.collapse_start && text.starts_with(is_class_separator);
  let has_trailing_space = !options.collapse_end && text.ends_with(is_class_separator);

  if !has_extra_whitespace(text, options) && is_sorted(class_names.clone()) {
    return Cow::Borrowed(text);
  }

  let mut result = String::with_capacity(text.len());
  if has_leading_space {
    result.push(' ');
  }
  if let Some(prefix) = prefix {
    push_class_name(&mut result, prefix);
  }
  let mut tailwind_classes = Vec::new();
  let mut ellipses = Vec::new();
  for class_name in class_names {
    if is_ellipsis(class_name) {
      ellipses.push(class_name);
    } else if let Some(class_info) = ClassInfo::from_class_name(class_name) {
      tailwind_classes.push(class_info);
    } else {
      push_class_name(&mut result, class_name);
    }
  }
  tailwind_classes.sort_unstable_by(compare_classes);
  // duplicates are next to each other because the comparison ends with the text
  tailwind_classes.dedup_by(|a, b| a.text == b.text);
  for class_name in tailwind_classes.iter().map(|class_info| class_info.text).chain(ellipses).chain(suffix) {
    push_class_name(&mut result, class_name);
  }
  if has_trailing_space {
    result.push(' ');
  }
  Cow::Owned(result)
}

fn is_class_separator(c: char) -> bool {
  matches!(c, ' ' | '\t' | '\r' | '\n' | '\x0C')
}

/// Gets if the text has whitespace that would be removed or replaced with a space.
fn has_extra_whitespace(text: &str, options: &SortOptions) -> bool {
  text.contains("  ")
    || text.contains(|c| c != ' ' && is_class_separator(c))
    || options.collapse_start && text.starts_with(' ')
    || options.collapse_end && text.ends_with(' ')
}

fn is_sorted<'a>(class_names: impl Iterator<Item = &'a str>) -> bool {
  let mut previous: Option<ClassInfo> = None;
  let mut has_ellipsis = false;
  for class_name in class_names {
    if is_ellipsis(class_name) {
      has_ellipsis = true;
      continue;
    }
    if has_ellipsis {
      return false;
    }
    match ClassInfo::from_class_name(class_name) {
      Some(class_info) => {
        if previous.is_some_and(|previous| compare_classes(&previous, &class_info) != Ordering::Less) {
          return false;
        }
        previous = Some(class_info);
      }
      None => {
        if previous.is_some() {
          return false;
        }
      }
    }
  }
  true
}

fn push_class_name(result: &mut String, class_name: &str) {
  if !result.is_empty() && !result.ends_with(' ') {
    result.push(' ');
  }
  result.push_str(class_name);
}

/// People use these as a placeholder for "and more classes", so they go last.
fn is_ellipsis(class_name: &str) -> bool {
  matches!(class_name, "..." | "…")
}

fn compare_classes(a: &ClassInfo, b: &ClassInfo) -> Ordering {
  a.arbitrary_variant_count
    .cmp(&b.arbitrary_variant_count)
    .then_with(|| a.arbitrary_variants().cmp(b.arbitrary_variants()))
    .then_with(|| (a.variant_weight != 0).cmp(&(b.variant_weight != 0)))
    .then_with(|| a.layer_index.cmp(&b.layer_index))
    .then_with(|| compare_variant_weights(a.variant_weight, b.variant_weight))
    .then_with(|| a.utility_index.cmp(&b.utility_index))
    .then_with(|| a.text.cmp(b.text))
}

/// Compares two sets of variant positions, where fewer variants go first and
/// otherwise the set with the earliest variant that the other doesn't have.
fn compare_variant_weights(a: u128, b: u128) -> Ordering {
  a.count_ones().cmp(&b.count_ones()).then_with(|| {
    let first_difference = (a ^ b).trailing_zeros();
    if a == b {
      Ordering::Equal
    } else if a & (1 << first_difference) != 0 {
      Ordering::Less
    } else {
      Ordering::Greater
    }
  })
}

#[derive(Clone, Copy)]
struct ClassInfo<'a> {
  text: &'a str,
  /// The text before the utility (ex. `hover:focus:` in `hover:focus:p-4`).
  variants_text: &'a str,
  arbitrary_variant_count: usize,
  /// Bit set of the positions of the non-arbitrary variants in `VARIANT_CLASSES`.
  variant_weight: u128,
  layer_index: usize,
  utility_index: usize,
}

impl<'a> ClassInfo<'a> {
  /// Gets the info for a class name or `None` when it's not a known Tailwind class.
  fn from_class_name(class_name: &'a str) -> Option<ClassInfo<'a>> {
    // class names are only separated on ASCII whitespace like in HTML, but
    // no Tailwind class has another kind of whitespace in it
    if class_name.contains(char::is_whitespace) {
      return None;
    }
    let (variants_text, utility) = split_utility(class_name)?;
    let utility_info = get_utility_info(utility)?;
    let mut arbitrary_variant_count = 0;
    let mut variant_weight = 0;
    for variant in split_variants(variants_text) {
      if variant.starts_with('[') {
        arbitrary_variant_count += 1;
      } else {
        variant_weight |= 1 << find_variant_position(variant).unwrap_or(UNKNOWN_VARIANT_POSITION);
      }
    }

    Some(ClassInfo {
      text: class_name,
      variants_text,
      arbitrary_variant_count,
      variant_weight,
      layer_index: utility_info.layer_index,
      utility_index: utility_info.utility_index,
    })
  }

  fn arbitrary_variants(&self) -> impl Iterator<Item = &'a str> {
    split_variants(self.variants_text).filter(|variant| variant.starts_with('['))
  }
}

/// Splits a class name into the text of its variants and its utility
/// (ex. `hover:focus:p-4` to `hover:focus:` and `p-4`).
fn split_utility(class_name: &str) -> Option<(&str, &str)> {
  let mut arbitrary_block_depth = 0;
  let mut utility_start = 0;
  for (index, byte) in class_name.bytes().enumerate() {
    match byte {
      b'[' => arbitrary_block_depth += 1,
      b']' => {
        if arbitrary_block_depth == 0 {
          return None;
        }
        arbitrary_block_depth -= 1;
      }
      b':' if arbitrary_block_depth == 0 => utility_start = index + 1,
      _ => {}
    }
  }
  if arbitrary_block_depth != 0 || utility_start == class_name.len() {
    return None;
  }
  Some(class_name.split_at(utility_start))
}

/// Splits on the colons that aren't in an arbitrary value (ex. not the one in `[&:hover]:`).
fn split_variants(variants_text: &str) -> impl Iterator<Item = &str> {
  let mut arbitrary_block_depth = 0;
  variants_text
    .split(move |c| {
      match c {
        '[' => arbitrary_block_depth += 1,
        ']' => arbitrary_block_depth -= 1,
        ':' => return arbitrary_block_depth == 0,
        _ => {}
      }
      false
    })
    .filter(|variant| !variant.is_empty())
}

#[derive(Clone, Copy)]
struct UtilityInfo {
  layer_index: usize,
  utility_index: usize,
}

fn get_utility_info(utility: &str) -> Option<UtilityInfo> {
  // the important modifier goes at the start in Tailwind 3 and at the end in Tailwind 4
  let utility = utility.strip_prefix('!').or_else(|| utility.strip_suffix('!')).unwrap_or(utility);
  if utility.starts_with('[') {
    return Some(UtilityInfo {
      layer_index: 2,
      utility_index: 0,
    });
  }
  let utility = utility.strip_prefix('-').unwrap_or(utility);
  utility_layers().iter().find_map(|layer| layer.get(utility))
}

fn utility_layers() -> &'static [UtilityLayer; 2] {
  static LAYERS: OnceLock<[UtilityLayer; 2]> = OnceLock::new();
  LAYERS.get_or_init(|| [UtilityLayer::new(0, COMPONENTS_LAYER_CLASSES), UtilityLayer::new(1, UTILITIES_LAYER_CLASSES)])
}

struct UtilityLayer {
  /// Utilities that match on their entire text (ex. `flex`).
  exact: FxHashMap<&'static str, UtilityInfo>,
  /// Utilities that are followed by a value (ex. `p-`).
  prefixes: FxHashMap<&'static str, UtilityInfo>,
}

impl UtilityLayer {
  fn new(layer_index: usize, layer_classes: &[&'static str]) -> Self {
    let mut layer = UtilityLayer {
      exact: Default::default(),
      prefixes: Default::default(),
    };
    for (utility_index, target) in layer_classes.iter().enumerate() {
      let info = UtilityInfo { layer_index, utility_index };
      match target.strip_suffix('$') {
        Some(target) => layer.exact.entry(target).or_insert(info),
        None => layer.prefixes.entry(target).or_insert(info),
      };
    }
    layer
  }

  fn get(&self, utility: &str) -> Option<UtilityInfo> {
    if let Some(info) = self.exact.get(utility) {
      return Some(*info);
    }
    // every prefix ends with a dash, so check the text up to each dash
    // starting from the last one in order to find the longest prefix
    utility
      .rmatch_indices('-')
      .filter(|(index, _)| index + 1 < utility.len())
      .find_map(|(index, _)| self.prefixes.get(&utility[..=index]).copied())
  }
}

/// The position of variants that aren't in `VARIANT_CLASSES`, which are ones from a
/// project's configuration or a newer Tailwind. These go after the known variants.
const UNKNOWN_VARIANT_POSITION: usize = u128::BITS as usize - 1;

fn find_variant_position(variant: &str) -> Option<usize> {
  let mut longest_match = None;
  let mut longest_match_len = 0;
  for (index, target) in VARIANT_CLASSES.iter().enumerate() {
    let Some(rest) = variant.strip_prefix(target) else {
      continue;
    };
    if rest.is_empty() || rest.starts_with("-[") {
      return Some(index);
    }
    // only match on a whole word in order to not match `small` to `sm`
    if rest.starts_with(['-', '/']) && target.len() > longest_match_len {
      longest_match = Some(index);
      longest_match_len = target.len();
    }
  }
  longest_match
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn tables_fit_the_lookups() {
    assert!(VARIANT_CLASSES.len() <= UNKNOWN_VARIANT_POSITION);
    for target in COMPONENTS_LAYER_CLASSES.iter().chain(UTILITIES_LAYER_CLASSES) {
      assert!(target.ends_with('$') || target.ends_with('-'), "{}", target);
      assert!(!target.starts_with('-'), "{}", target);
    }
  }

  #[test]
  fn sorts_unknown_classes_first() {
    assert_sorts("px-2 foo p-4 bar", "foo bar p-4 px-2");
  }

  #[test]
  fn sorts_variants_after_plain_utilities() {
    assert_sorts("hover:focus:m-2 foo hover:px-2 p-4", "foo p-4 hover:px-2 hover:focus:m-2");
  }

  #[test]
  fn handles_colons_in_arbitrary_segments() {
    assert_sorts("[&:hover]:p-4 flex", "flex [&:hover]:p-4");
    assert_sorts(r"[&>.a\_p]:after:content-['\2'] [&>.a\_p]:z-0", r"[&>.a\_p]:z-0 [&>.a\_p]:after:content-['\2']");
  }

  #[test]
  fn sorts_important_utilities_with_their_utility() {
    assert_sorts("flex !p-4 m-2", "m-2 flex !p-4");
    assert_sorts("flex p-4! m-2", "m-2 flex p-4!");
    assert_sorts("hover:!p-4 !flex", "!flex hover:!p-4");
  }

  #[test]
  fn sorts_unknown_variants_after_known_variants() {
    assert_sorts("custom:flex p-4", "p-4 custom:flex");
    assert_sorts("small:p-4 hover:flex hoverable:p-4 m-2", "m-2 hover:flex hoverable:p-4 small:p-4");
    assert_sorts("custom:hover:p-4 hover:focus:p-4 custom:m-2", "custom:m-2 hover:focus:p-4 custom:hover:p-4");
    assert_sorts("group-hover/item:p-4 peer-checked:m-2 m-2", "m-2 group-hover/item:p-4 peer-checked:m-2");
  }

  #[test]
  fn borrows_when_already_sorted() {
    for text in ["", " ", "foo", "foo bar foo", "flex p-4", "foo bar p-4 px-2 hover:px-2 ...", "{{ 'p-4 flex' }}"] {
      assert!(
        matches!(sort_class_names(text, &Default::default()), Cow::Borrowed(result) if result == text),
        "{}",
        text
      );
    }
    let options = SortOptions {
      collapse_start: false,
      collapse_end: false,
      ..Default::default()
    };
    assert!(matches!(sort_class_names(" flex p-4 ", &options), Cow::Borrowed(_)));
  }

  // The tests below are ported from prettier-plugin-tailwindcss.

  #[test]
  fn sorts_like_the_prettier_sorter() {
    assert_sorts("sm:bg-tomato bg-red-500", "bg-red-500 sm:bg-tomato");
    assert_sorts("p-4 m-2", "m-2 p-4");
    assert_sorts("hover:text-red-500 text-blue-500", "text-blue-500 hover:text-red-500");
    assert_sorts("sm:p-0 p-0", "p-0 sm:p-0");
  }

  #[test]
  fn collapses_whitespace() {
    assert_sorts("  sm:bg-tomato   bg-red-500  ", "bg-red-500 sm:bg-tomato");
    assert_sorts("  sm:p-0   p-0 ", "p-0 sm:p-0");
    assert_sorts("   m-0  sm:p-0  p-0   ", "m-0 p-0 sm:p-0");
    assert_sorts(" sm:p-0\n  p-0   ", "p-0 sm:p-0");
    assert_sorts("  ", " ");
    assert_sorts("\n\t", " ");
  }

  #[test]
  fn only_separates_on_ascii_whitespace() {
    assert_sorts("block px-1\u{3000}py-2", "px-1\u{3000}py-2 block");
  }

  #[test]
  fn removes_duplicate_tailwind_classes() {
    assert_sorts("bg-red-500 sm:bg-tomato bg-red-500", "bg-red-500 sm:bg-tomato");
    assert_sorts("sm:p-0 p-0 p-0", "p-0 sm:p-0");
    assert_sorts("flex flex", "flex");
    assert_sorts("   flex  flex ", "flex");
  }

  #[test]
  fn keeps_duplicate_unknown_classes() {
    assert_sorts(
      "idonotexist sm:p-0 p-0 idonotexist p-0 idonotexist",
      "idonotexist idonotexist idonotexist p-0 sm:p-0",
    );
  }

  #[test]
  fn moves_ellipsis_to_the_end() {
    assert_sorts("... sm:p-0 p-0", "p-0 sm:p-0 ...");
    assert_sorts("… sm:p-0 p-0", "p-0 sm:p-0 …");
    assert_sorts("sm:p-0 ... p-0", "p-0 sm:p-0 ...");
    assert_sorts("sm:p-0 … p-0", "p-0 sm:p-0 …");
    assert_sorts("sm:p-0 p-0 ...", "p-0 sm:p-0 ...");
    assert_sorts("sm:p-0 p-0 …", "p-0 sm:p-0 …");
  }

  #[test]
  fn ignores_class_attributes_containing_a_template() {
    assert_sorts("sm:p-0 p-0 {{ 'p-0 sm:p-0 m-0' }}", "sm:p-0 p-0 {{ 'p-0 sm:p-0 m-0' }}");
  }

  #[test]
  fn keeps_the_first_and_last_class_when_ignored() {
    let ignore_first = SortOptions {
      ignore_first: true,
      ..Default::default()
    };
    let ignore_last = SortOptions {
      ignore_last: true,
      collapse_end: false,
      ..Default::default()
    };
    let ignore_both = SortOptions {
      ignore_first: true,
      ignore_last: true,
      collapse_start: false,
      collapse_end: false,
    };
    assert_eq!(sort_class_names("sm:block inline flex", &ignore_last), "inline sm:block flex");
    assert_eq!(sort_class_names("sm:block md:inline flex", &ignore_first), "sm:block flex md:inline");
    assert_eq!(sort_class_names("   flex  flex flex", &ignore_last), "flex flex");
    assert_eq!(sort_class_names("block block", &ignore_first), "block block");
    assert_eq!(sort_class_names("a sm:p-0 p-0 b", &ignore_both), "a p-0 sm:p-0 b");
    assert_eq!(sort_class_names("a", &ignore_both), "a");
    assert_eq!(sort_class_names("a b", &ignore_both), "a b");
  }

  #[test]
  fn keeps_a_space_when_not_collapsing_an_end() {
    let keep_end = SortOptions {
      collapse_end: false,
      ..Default::default()
    };
    let keep_start = SortOptions {
      collapse_start: false,
      ..Default::default()
    };
    assert_eq!(sort_class_names("sm:p-0 p-0  ", &keep_end), "p-0 sm:p-0 ");
    assert_eq!(sort_class_names("  sm:p-0 p-0  ", &keep_end), "p-0 sm:p-0 ");
    assert_eq!(sort_class_names("  sm:p-0 p-0  ", &keep_start), " p-0 sm:p-0");
    assert_eq!(sort_class_names(" aspect-square w-full", &keep_start), " aspect-square w-full");
    assert_eq!(sort_class_names(" min-h-0 grow basis-0", &keep_start), " min-h-0 grow basis-0");
    assert_eq!(sort_class_names("flex ", &keep_end), "flex ");
  }

  #[track_caller]
  fn assert_sorts(text: &str, expected: &str) {
    assert_eq!(sort_class_names(text, &Default::default()), expected);
  }
}
