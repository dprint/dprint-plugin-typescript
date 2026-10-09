//! Sorts Tailwind CSS class lists.
//!
//! This module is self-contained so that it can be extracted into its own crate.
//!
//! The handling of whitespace, duplicates, unknown classes, and partially sorted
//! class lists is patterned off prettier-plugin-tailwindcss and its tests. The order
//! of the classes is the one Tailwind CSS outputs its CSS in, where the comparisons
//! are ported from Tailwind and `tables.rs` is generated from it. Both projects are
//! MIT licensed (see `LICENSE.prettier-plugin-tailwindcss` and `LICENSE.tailwindcss`).
//!
//! Unlike the Prettier plugin, this doesn't load Tailwind or a project's configuration,
//! so it only knows about Tailwind's default theme:
//!
//! - A value that's not in the default theme (ex. `text-brand`) gets the position
//!   that most values of its utility have (a color for `text-`).
//! - A variant that's not in Tailwind (ex. `custom:flex`) goes after the ones that are.

mod tables;

use std::borrow::Cow;
use std::cmp::Ordering;

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

/// Compares the same way as Tailwind does when it sorts the CSS it generates.
fn compare_classes(a: &ClassInfo, b: &ClassInfo) -> Ordering {
  a.variants
    .cmp(&b.variants)
    .then_with(|| a.position.cmp(&b.position))
    .then_with(|| compare_text(a.text, b.text))
}

/// Compares two strings where the numbers in them are compared as numbers.
fn compare_text(a: &str, b: &str) -> Ordering {
  let (a, b) = (a.as_bytes(), b.as_bytes());
  for index in 0..a.len().min(b.len()) {
    if a[index].is_ascii_digit() && b[index].is_ascii_digit() {
      let a_number = get_leading_digits(&a[index..]);
      let b_number = get_leading_digits(&b[index..]);
      let ordering = compare_numbers(a_number, b_number).then_with(|| a_number.cmp(b_number));
      if ordering != Ordering::Equal {
        return ordering;
      }
    } else if a[index] != b[index] {
      return a[index].cmp(&b[index]);
    }
  }
  a.len().cmp(&b.len())
}

fn get_leading_digits(text: &[u8]) -> &[u8] {
  let len = text.iter().position(|byte| !byte.is_ascii_digit()).unwrap_or(text.len());
  &text[..len]
}

fn compare_numbers(a: &[u8], b: &[u8]) -> Ordering {
  let a = &a[a.iter().position(|byte| *byte != b'0').unwrap_or(a.len())..];
  let b = &b[b.iter().position(|byte| *byte != b'0').unwrap_or(b.len())..];
  a.len().cmp(&b.len()).then_with(|| a.cmp(b))
}

#[derive(Clone, Copy)]
struct ClassInfo<'a> {
  text: &'a str,
  variants: ClassVariants<'a>,
  /// Position of the utility among the other utilities.
  position: u16,
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
    let position = get_utility_position(utility)?;
    let mut variants = ClassVariants::default();
    for variant in split_top_level(variants_text, ':').filter(|variant| !variant.is_empty()) {
      variants.insert(variant)?;
    }

    Some(ClassInfo {
      text: class_name,
      variants,
      position,
    })
  }
}

/// Splits a class name into the text of its variants and its utility
/// (ex. `hover:focus:p-4` to `hover:focus:` and `p-4`).
fn split_utility(class_name: &str) -> Option<(&str, &str)> {
  let mut arbitrary_block_depth = 0;
  let mut utility_start = 0;
  for (index, byte) in class_name.bytes().enumerate() {
    match byte {
      b'[' | b'(' => arbitrary_block_depth += 1,
      b']' | b')' => {
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

/// Splits on the separators that aren't in an arbitrary value (ex. not the colon in `[&:hover]:`).
fn split_top_level(text: &str, separator: char) -> impl Iterator<Item = &str> {
  let mut arbitrary_block_depth = 0;
  text.split(move |c| {
    match c {
      '[' | '(' => arbitrary_block_depth += 1,
      ']' | ')' => arbitrary_block_depth -= 1,
      _ => return c == separator && arbitrary_block_depth == 0,
    }
    false
  })
}

/// Splits off what follows the last slash (ex. `bg-red-500/50` to `bg-red-500` and `50`).
fn split_modifier(text: &str) -> (&str, Option<&str>) {
  match split_top_level(text, '/').last() {
    Some(modifier) if modifier.len() < text.len() => (&text[..text.len() - modifier.len() - 1], Some(modifier)),
    _ => (text, None),
  }
}

/// Gets the indexes of the dashes that may separate a root from its value starting from the last one.
fn value_separator_indexes(text: &str) -> impl Iterator<Item = usize> + '_ {
  let end = text.find(['[', '(']).unwrap_or(text.len());
  text[..end].rmatch_indices('-').map(|(index, _)| index).filter(|index| index + 1 < text.len())
}

fn get_utility_position(utility: &str) -> Option<u16> {
  // the important modifier goes at the start in Tailwind 3 and at the end in Tailwind 4
  let utility = utility.strip_prefix('!').or_else(|| utility.strip_suffix('!')).unwrap_or(utility);
  if let Some(declaration) = utility.strip_prefix('[') {
    // an arbitrary property (ex. `[color:red]`)
    let (property, _) = declaration.split_once(':')?;
    return Some(
      find(PROPERTIES, property, |entry| entry.0)
        .map(|entry| entry.1)
        .unwrap_or(UNKNOWN_PROPERTY_POSITION),
    );
  }
  // negative utilities have the position of the positive one
  let utility = utility.strip_prefix('-').unwrap_or(utility);
  let (utility_without_modifier, _) = split_modifier(utility);
  if let Some(entry) = find(UTILITIES, utility, |entry| entry.0).or_else(|| find(UTILITIES, utility_without_modifier, |entry| entry.0)) {
    return Some(entry.1);
  }
  let utility = utility_without_modifier;
  value_separator_indexes(utility).find_map(|index| {
    let (_, default_position, length_position, color_position) = find(UTILITY_ROOTS, &utility[..index], |entry| entry.0)?;
    Some(match get_arbitrary_value_kind(&utility[index + 1..]) {
      Some(ArbitraryValueKind::Length) => *length_position,
      Some(ArbitraryValueKind::Color) => *color_position,
      None => *default_position,
    })
  })
}

enum ArbitraryValueKind {
  Length,
  Color,
}

/// Guesses what an arbitrary value is for the roots that are for more than one
/// CSS property (ex. `text-[14px]` sets the font size and `text-[#fff]` the color).
fn get_arbitrary_value_kind(value: &str) -> Option<ArbitraryValueKind> {
  let value = value.strip_prefix('[')?;
  if let Some((data_type, _)) = value.split_once(':') {
    return match data_type {
      "length" | "percentage" | "number" | "absolute-size" | "relative-size" => Some(ArbitraryValueKind::Length),
      "color" => Some(ArbitraryValueKind::Color),
      _ => None,
    };
  }
  let is_number = value.trim_start_matches(['-', '+', '.']).starts_with(|c: char| c.is_ascii_digit());
  if is_number || value.starts_with("calc(") {
    Some(ArbitraryValueKind::Length)
  } else if ["#", "rgb", "hsl", "hwb", "lab(", "lch(", "oklab(", "oklch(", "color("]
    .iter()
    .any(|prefix| value.starts_with(prefix))
  {
    Some(ArbitraryValueKind::Color)
  } else {
    None
  }
}

/// Finds an entry in a table that's sorted by name.
fn find<'a, T>(table: &'a [T], name: &str, get_name: impl Fn(&T) -> &'static str) -> Option<&'a T> {
  table.binary_search_by(|entry| get_name(entry).cmp(name)).ok().map(|index| &table[index])
}

/// The most variants a class may have, where classes with more are not sorted.
const MAX_VARIANTS: usize = 8;

/// The variants of a class ordered from the last one Tailwind would output to the first.
///
/// Tailwind gives each variant a bit based on its position then compares the numbers
/// those make, which is the same as comparing these.
#[derive(Clone, Copy, Default)]
struct ClassVariants<'a> {
  items: [&'a str; MAX_VARIANTS],
  len: usize,
}

impl<'a> ClassVariants<'a> {
  fn insert(&mut self, variant: &'a str) -> Option<()> {
    let mut index = 0;
    while index < self.len {
      match compare_variants(variant, self.items[index]) {
        Ordering::Greater => break,
        Ordering::Equal => return Some(()),
        Ordering::Less => index += 1,
      }
    }
    if self.len == MAX_VARIANTS {
      return None;
    }
    self.items.copy_within(index..self.len, index + 1);
    self.items[index] = variant;
    self.len += 1;
    Some(())
  }

  fn cmp(&self, other: &ClassVariants) -> Ordering {
    for (a, b) in self.items[..self.len].iter().zip(&other.items[..other.len]) {
      let ordering = compare_variants(a, b);
      if ordering != Ordering::Equal {
        return ordering;
      }
    }
    self.len.cmp(&other.len)
  }
}

/// Compares the same way as Tailwind's `Variants#compare`.
fn compare_variants(a: &str, b: &str) -> Ordering {
  // arbitrary variants (ex. `[&>*]`) go last
  match (a.starts_with('['), b.starts_with('[')) {
    (true, true) => return a.cmp(b),
    (true, false) => return Ordering::Greater,
    (false, true) => return Ordering::Less,
    (false, false) => {}
  }

  let (a, b) = (VariantInfo::parse(a), VariantInfo::parse(b));
  let ordering = a.order.cmp(&b.order);
  if ordering != Ordering::Equal {
    return ordering;
  }
  if a.kind == Some(VariantKind::Compound) && b.kind == Some(VariantKind::Compound) {
    return compare_variants(a.value.unwrap_or(""), b.value.unwrap_or("")).then_with(|| a.modifier.cmp(&b.modifier));
  }
  if a.value_order != VariantValueOrder::None {
    let is_ascending = a.value_order == VariantValueOrder::Ascending;
    let ordering = match (a.breakpoint_value(), b.breakpoint_value()) {
      (Some(a_value), Some(b_value)) => return compare_breakpoints(a_value, b_value, is_ascending),
      (Some(_), None) => Ordering::Greater,
      (None, Some(_)) => Ordering::Less,
      (None, None) => return a.text.cmp(b.text),
    };
    return if is_ascending { ordering } else { ordering.reverse() };
  }
  a.root.cmp(b.root).then_with(|| match (a.value, b.value) {
    // named values go before arbitrary ones
    (Some(a_value), Some(b_value)) => a_value.starts_with('[').cmp(&b_value.starts_with('[')).then_with(|| a_value.cmp(b_value)),
    (a_value, b_value) => a_value.cmp(&b_value),
  })
}

/// Compares the same way as Tailwind's `compareBreakpoints`.
fn compare_breakpoints(a: &str, b: &str, is_ascending: bool) -> Ordering {
  // values are only comparable when they have the same unit or CSS function
  fn bucket(value: &str) -> impl Iterator<Item = char> + '_ {
    let function_name_end = value.find('(');
    value[..function_name_end.unwrap_or(value.len())]
      .chars()
      .filter(move |c| function_name_end.is_some() || !c.is_ascii_digit() && *c != '.')
  }

  fn parse_int(value: &str) -> Option<i64> {
    let digits_start = value.find(|c: char| !matches!(c, '-' | '+')).filter(|index| *index <= 1)?;
    let digits_len = value[digits_start..].find(|c: char| !c.is_ascii_digit()).unwrap_or(value.len() - digits_start);
    value[..digits_start + digits_len].parse().ok()
  }

  if a == b {
    return Ordering::Equal;
  }
  bucket(a).cmp(bucket(b)).then_with(|| match (parse_int(a), parse_int(b)) {
    (Some(a_number), Some(b_number)) if is_ascending => a_number.cmp(&b_number),
    (Some(a_number), Some(b_number)) => b_number.cmp(&a_number),
    _ => a.cmp(b),
  })
}

/// The order of variants that aren't in `VARIANTS`, which are ones from a project's
/// configuration or a newer Tailwind. These go after the known variants.
const UNKNOWN_VARIANT_ORDER: u16 = u16::MAX;

struct VariantInfo<'a> {
  /// The text of the variant without its modifier.
  text: &'a str,
  root: &'a str,
  value: Option<&'a str>,
  modifier: Option<&'a str>,
  kind: Option<VariantKind>,
  order: u16,
  value_order: VariantValueOrder,
}

impl<'a> VariantInfo<'a> {
  fn parse(variant: &'a str) -> VariantInfo<'a> {
    let (text, modifier) = split_modifier(variant);
    let found = find(VARIANTS, text, |entry| entry.0).map(|entry| (entry, None)).or_else(|| {
      // container query variants don't have a dash before their value (ex. `@md`)
      let container_value = text.strip_prefix('@').filter(|value| !value.starts_with("max-") && !value.starts_with("min-"));
      let mut roots_and_values = value_separator_indexes(text)
        .map(|index| (&text[..index], &text[index + 1..]))
        .chain(container_value.map(|value| ("@", value)));
      roots_and_values.find_map(|(root, value)| {
        let entry = find(VARIANTS, root, |entry| entry.0).filter(|entry| entry.1 != VariantKind::Static)?;
        Some((entry, Some(value)))
      })
    });
    match found {
      Some(((root, kind, order, value_order), value)) => VariantInfo {
        text,
        root,
        value,
        modifier,
        kind: Some(*kind),
        order: *order,
        value_order: *value_order,
      },
      None => VariantInfo {
        text,
        root: text,
        value: None,
        modifier,
        kind: None,
        order: UNKNOWN_VARIANT_ORDER,
        value_order: VariantValueOrder::None,
      },
    }
  }

  /// Gets the value of a variant that's ordered by its value (ex. `48rem` for `md` or `min-[48rem]`).
  fn breakpoint_value(&self) -> Option<&'a str> {
    match self.value.and_then(|value| value.strip_prefix('[')) {
      Some(value) => value.strip_suffix(']').filter(|value| !value.contains("var(")),
      None => find(VARIANT_VALUES, self.text, |entry| entry.0).map(|entry| entry.1),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn tables_are_sorted_for_searching() {
    assert!(UTILITIES.is_sorted_by(|a, b| a.0 < b.0));
    assert!(UTILITY_ROOTS.is_sorted_by(|a, b| a.0 < b.0));
    assert!(PROPERTIES.is_sorted_by(|a, b| a.0 < b.0));
    assert!(VARIANTS.is_sorted_by(|a, b| a.0 < b.0));
    assert!(VARIANT_VALUES.is_sorted_by(|a, b| a.0 < b.0));
  }

  #[test]
  fn sorts_the_same_as_tailwind() {
    let mut failures = Vec::new();
    let mut count = 0;
    for line in include_str!("sort_tests.txt").lines().filter(|line| !line.starts_with('#')) {
      let (text, expected) = line.split_once(" => ").unwrap();
      let actual = sort_class_names(text, &Default::default());
      count += 1;
      if actual != expected {
        failures.push(format!(
          "   input: {}
expected: {}
  actual: {}",
          text, expected, actual
        ));
      }
    }
    assert!(
      failures.is_empty(),
      "{} of {} failed:

{}",
      failures.len(),
      count,
      failures[..failures.len().min(10)].join(
        "

"
      )
    );
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
    assert_sorts(
      "custom:hover:p-4 [&>*]:p-4 hover:focus:p-4 custom:m-2",
      "hover:focus:p-4 custom:m-2 custom:hover:p-4 [&>*]:p-4",
    );
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
