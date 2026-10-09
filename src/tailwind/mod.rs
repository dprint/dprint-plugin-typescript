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
//! Unlike the Prettier plugin, this doesn't load Tailwind or a project's CSS, so it
//! only knows about Tailwind's default theme and what it's told with a `Project`.
//! A value that's in neither (ex. `text-brand`) gets the position that most values
//! of its utility have (a color for `text-`).

mod tables;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::ops::Bound;

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
  /// Keeps the whitespace between the classes as is, which makes the other
  /// options for whitespace not do anything.
  pub preserve_whitespace: bool,
  /// Keeps duplicate classes.
  pub preserve_duplicates: bool,
}

impl Default for SortOptions {
  fn default() -> Self {
    Self {
      ignore_first: false,
      ignore_last: false,
      collapse_start: true,
      collapse_end: true,
      preserve_whitespace: false,
      preserve_duplicates: false,
    }
  }
}

/// How a project customizes Tailwind, which is what it has in its CSS file.
#[derive(Debug, Clone, Copy)]
pub struct Project<'a> {
  /// The prefix of every class (ex. `tw` when classes look like `tw:flex`).
  pub prefix: Option<&'a str>,
  /// The theme variables and their values (ex. `--breakpoint-3xl` and `120rem`).
  pub theme: &'a BTreeMap<String, String>,
  /// The names of the custom variants in the order they're defined.
  pub variants: &'a [String],
  /// The names of the custom utilities (ex. `btn` or `tab-*` for one that has
  /// a value) along with the CSS properties each one sets.
  pub utilities: &'a BTreeMap<String, Vec<String>>,
}

impl Default for Project<'_> {
  fn default() -> Self {
    static THEME: BTreeMap<String, String> = BTreeMap::new();
    static UTILITIES: BTreeMap<String, Vec<String>> = BTreeMap::new();
    Self {
      prefix: None,
      theme: &THEME,
      variants: &[],
      utilities: &UTILITIES,
    }
  }
}

/// Sorts the class names in the provided text in the order Tailwind CSS recommends.
///
/// Unknown classes go first in their original order, whitespace is collapsed, and
/// duplicate Tailwind classes are removed. Borrows the text when it doesn't change.
pub fn sort_class_names<'a>(text: &'a str, options: &SortOptions, project: &Project) -> Cow<'a, str> {
  // matches what Prettier does for class attributes containing a template
  if text.is_empty() || text.contains("{{") {
    return Cow::Borrowed(text);
  }
  if options.preserve_whitespace {
    return sort_class_names_preserving_whitespace(text, options, project);
  }
  let mut class_names = text.split(is_class_separator).filter(|class_name| !class_name.is_empty());
  let Some(first) = class_names.next() else {
    return if text == " " { Cow::Borrowed(text) } else { Cow::Borrowed(" ") };
  };
  let (ignored_first, first) = if options.ignore_first { (Some(first), None) } else { (None, Some(first)) };
  let ignored_last = if options.ignore_last { class_names.next_back() } else { None };
  let class_names = first.into_iter().chain(class_names);
  let has_leading_space = !options.collapse_start && text.starts_with(is_class_separator);
  let has_trailing_space = !options.collapse_end && text.ends_with(is_class_separator);

  if !has_extra_whitespace(text, options) && is_sorted(class_names.clone(), options, project) {
    return Cow::Borrowed(text);
  }

  let mut result = String::with_capacity(text.len());
  if has_leading_space {
    result.push(' ');
  }
  if let Some(class_name) = ignored_first {
    push_class_name(&mut result, class_name);
  }
  let mut sorted_classes = SortedClasses::new(class_names, project);
  if !options.preserve_duplicates {
    sorted_classes.tailwind.dedup_by(|a, b| a.text == b.text);
  }
  for class_name in sorted_classes.iter().chain(ignored_last) {
    push_class_name(&mut result, class_name);
  }
  if has_trailing_space {
    result.push(' ');
  }
  Cow::Owned(result)
}

fn sort_class_names_preserving_whitespace<'a>(text: &'a str, options: &SortOptions, project: &Project) -> Cow<'a, str> {
  // split the text into its leading whitespace then each class with the whitespace after it
  let leading_whitespace_len = text.len() - text.trim_start_matches(is_class_separator).len();
  let mut items = Vec::new();
  let mut rest = &text[leading_whitespace_len..];
  while !rest.is_empty() {
    let (class_name, after) = rest.split_at(rest.find(is_class_separator).unwrap_or(rest.len()));
    let (whitespace, after) = after.split_at(after.len() - after.trim_start_matches(is_class_separator).len());
    items.push((class_name, whitespace));
    rest = after;
  }
  let start = (options.ignore_first as usize).min(items.len());
  let end = items.len() - (options.ignore_last as usize).min(items.len() - start);
  let sorted_classes = SortedClasses::new(items[start..end].iter().map(|(class_name, _)| *class_name), project);

  // the whitespace stays where it is and the classes get moved around it
  let mut result = String::with_capacity(text.len());
  result.push_str(&text[..leading_whitespace_len]);
  let mut previous: Option<&str> = None;
  let mut pending_whitespace = "";
  let class_names = items[..start].iter().map(|(class_name, _)| *class_name);
  let class_names = class_names
    .chain(sorted_classes.iter())
    .chain(items[end..].iter().map(|(class_name, _)| *class_name));
  for (index, class_name) in class_names.enumerate() {
    let is_sorted_class = index >= start && index < end;
    let is_duplicate = is_sorted_class && !options.preserve_duplicates && previous == Some(class_name) && sorted_classes.is_tailwind(class_name);
    // the whitespace before a removed duplicate is removed with it
    if !is_duplicate {
      result.push_str(pending_whitespace);
      result.push_str(class_name);
    }
    pending_whitespace = items[index].1;
    previous = is_sorted_class.then_some(class_name);
  }
  result.push_str(pending_whitespace);
  if result == text {
    Cow::Borrowed(text)
  } else {
    Cow::Owned(result)
  }
}

/// Gets if the character separates class names, which is only ASCII whitespace like in HTML.
pub fn is_class_separator(c: char) -> bool {
  matches!(c, ' ' | '\t' | '\r' | '\n' | '\x0C')
}

/// Gets if the text has whitespace that would be removed or replaced with a space.
fn has_extra_whitespace(text: &str, options: &SortOptions) -> bool {
  text.contains("  ")
    || text.contains(|c| c != ' ' && is_class_separator(c))
    || options.collapse_start && text.starts_with(' ')
    || options.collapse_end && text.ends_with(' ')
}

fn is_sorted<'a>(class_names: impl Iterator<Item = &'a str>, options: &SortOptions, project: &Project) -> bool {
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
    match ClassInfo::from_class_name(class_name, project) {
      Some(class_info) => {
        let is_in_order = previous.is_none_or(|previous| match compare_classes(&previous, &class_info, project) {
          Ordering::Less => true,
          Ordering::Equal => options.preserve_duplicates,
          Ordering::Greater => false,
        });
        if !is_in_order {
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

/// Class names in the order they get output in, which is the ones that aren't
/// Tailwind classes in their original order, then the Tailwind ones, then ellipses.
struct SortedClasses<'a> {
  unknown: Vec<&'a str>,
  tailwind: Vec<ClassInfo<'a>>,
  ellipses: Vec<&'a str>,
}

impl<'a> SortedClasses<'a> {
  fn new(class_names: impl Iterator<Item = &'a str>, project: &Project) -> Self {
    let mut result = SortedClasses {
      unknown: Vec::new(),
      tailwind: Vec::new(),
      ellipses: Vec::new(),
    };
    for class_name in class_names {
      if is_ellipsis(class_name) {
        result.ellipses.push(class_name);
      } else if let Some(class_info) = ClassInfo::from_class_name(class_name, project) {
        result.tailwind.push(class_info);
      } else {
        result.unknown.push(class_name);
      }
    }
    // duplicates end up next to each other because the comparison ends with the text
    result.tailwind.sort_unstable_by(|a, b| compare_classes(a, b, project));
    result
  }

  fn iter(&self) -> impl Iterator<Item = &'a str> + '_ {
    let tailwind = self.tailwind.iter().map(|class_info| class_info.text);
    self.unknown.iter().copied().chain(tailwind).chain(self.ellipses.iter().copied())
  }

  fn is_tailwind(&self, class_name: &str) -> bool {
    self.tailwind.iter().any(|class_info| class_info.text == class_name)
  }
}

/// Compares the same way as Tailwind does when it sorts the CSS it generates.
fn compare_classes(a: &ClassInfo, b: &ClassInfo, project: &Project) -> Ordering {
  a.variants
    .cmp(&b.variants, project)
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
  fn from_class_name(class_name: &'a str, project: &Project) -> Option<ClassInfo<'a>> {
    // class names are only separated on ASCII whitespace like in HTML, but
    // no Tailwind class has another kind of whitespace in it
    if class_name.contains(char::is_whitespace) {
      return None;
    }
    let class_name_without_prefix = match project.prefix {
      Some(prefix) => class_name.strip_prefix(prefix)?.strip_prefix(':')?,
      None => class_name,
    };
    let (variants_text, utility) = split_utility(class_name_without_prefix)?;
    let position = get_utility_position(utility, project)?;
    let mut variants = ClassVariants::default();
    if let Some(variants_text) = variants_text.strip_suffix(':') {
      for variant in split_top_level(variants_text, ':') {
        variants.insert(variant, project)?;
      }
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
  let mut arbitrary_blocks = ArbitraryBlocks::default();
  let mut utility_start = 0;
  for (index, c) in class_name.char_indices() {
    if arbitrary_blocks.is_outside(c) && c == ':' {
      utility_start = index + 1;
    }
  }
  if !arbitrary_blocks.is_balanced() || utility_start == class_name.len() {
    return None;
  }
  Some(class_name.split_at(utility_start))
}

/// Splits on the separators that aren't in an arbitrary value (ex. not the colon in `[&:hover]:`).
fn split_top_level(text: &str, separator: char) -> impl Iterator<Item = &str> {
  let mut arbitrary_blocks = ArbitraryBlocks::default();
  text.split(move |c| arbitrary_blocks.is_outside(c) && c == separator)
}

/// Keeps track of being in an arbitrary value while going over the characters of a
/// class name (ex. the `&:hover` in `[&:hover]:flex`). The brackets that are in quotes
/// or escaped are part of the value like they are in Tailwind (ex. `content-[')']`).
#[derive(Default)]
struct ArbitraryBlocks {
  depth: usize,
  quote: Option<char>,
  is_escaped: bool,
  has_unopened_close: bool,
}

impl ArbitraryBlocks {
  /// Moves past the character and gets if it's outside of the arbitrary values.
  fn is_outside(&mut self, c: char) -> bool {
    if self.is_escaped {
      self.is_escaped = false;
      return false;
    }
    match (self.quote, c) {
      (_, '\\') => self.is_escaped = true,
      (Some(quote), _) => {
        if c == quote {
          self.quote = None;
        }
      }
      (None, '"' | '\'') => self.quote = Some(c),
      (None, '[' | '(') => self.depth += 1,
      (None, ']' | ')') => match self.depth.checked_sub(1) {
        Some(depth) => self.depth = depth,
        None => self.has_unopened_close = true,
      },
      (None, _) => return self.depth == 0,
    }
    false
  }

  fn is_balanced(&self) -> bool {
    self.depth == 0 && !self.has_unopened_close
  }
}

/// Splits off what follows the last slash (ex. `bg-red-500/50` to `bg-red-500` and `50`).
fn split_modifier(text: &str) -> (&str, Option<&str>) {
  match split_top_level(text, '/').last() {
    Some(modifier) if modifier.len() < text.len() => (&text[..text.len() - modifier.len() - 1], Some(modifier)),
    _ => (text, None),
  }
}

/// Gets the roots and values the text may be made of starting from the longest
/// root (ex. `border-x` and `2` then `border` and `x-2` for `border-x-2`).
fn roots_and_values(text: &str) -> impl Iterator<Item = (&str, &str)> {
  let end = text.find(['[', '(']).unwrap_or(text.len());
  text[..end]
    .rmatch_indices('-')
    .filter(|(index, _)| index + 1 < text.len())
    .map(|(index, _)| (&text[..index], &text[index + 1..]))
}

fn get_utility_position(utility: &str, project: &Project) -> Option<u16> {
  // the important modifier goes at the start in Tailwind 3 and at the end in Tailwind 4
  let utility = utility.strip_prefix('!').or_else(|| utility.strip_suffix('!')).unwrap_or(utility);
  let (utility_without_modifier, modifier) = split_modifier(utility);
  let Some(modifier) = modifier else {
    return find_utility(utility, project).map(|(position, _)| position);
  };
  // what's after the slash is part of the value when it's a fraction (ex. `w-1/2`)
  if let Some((position, _)) = find_tailwind_utility(utility, project) {
    return Some(position);
  }
  let (position, modifier_kind) = find_utility(utility_without_modifier, project)?;
  is_valid_modifier(modifier, modifier_kind, project).then_some(position)
}

/// Gets the position of a utility that doesn't have a modifier and the
/// modifier it may have, or `None` when it's not a utility.
fn find_utility(utility: &str, project: &Project) -> Option<(u16, Modifier)> {
  if let Some(declaration) = utility.strip_prefix('[') {
    // an arbitrary property (ex. `[color:red]`)
    let (property, _) = declaration.strip_suffix(']')?.split_once(':')?;
    return Some((get_properties_position(std::iter::once(property), 1), Modifier::Opacity));
  }
  if let Some(position) = get_custom_utility_position(utility, project) {
    return Some((position, Modifier::None));
  }
  find_tailwind_utility(utility, project)
}

/// Finds a utility that's one of Tailwind's.
fn find_tailwind_utility(utility: &str, project: &Project) -> Option<(u16, Modifier)> {
  if let Some(entry) = find(UTILITIES, utility, |entry| entry.0) {
    return Some((entry.1, entry.2));
  }
  roots_and_values(utility).find_map(|(root, value)| {
    let root = find(UTILITY_ROOTS, root, |root| root.name)?;
    if value.starts_with(['[', '(']) {
      let is_arbitrary = root.value_kinds & VALUE_ARBITRARY != 0 && is_arbitrary_value(value);
      return is_arbitrary.then(|| match get_arbitrary_value_kind(value) {
        Some(ArbitraryValueKind::Length) => root.arbitrary_length,
        Some(ArbitraryValueKind::Color) => root.arbitrary_color,
        None => root.default,
      });
    }
    let has_kind = |kind: u8, is_kind: fn(&str) -> bool| root.value_kinds & kind != 0 && is_kind(value);
    let is_number = has_kind(VALUE_INTEGER, is_integer)
      || has_kind(VALUE_QUARTER, is_quarter)
      || has_kind(VALUE_DECIMAL, is_decimal)
      || has_kind(VALUE_FRACTION, is_fraction)
      || has_kind(VALUE_PERCENTAGE, is_percentage);
    if is_number {
      return Some(root.number);
    }
    let is_named = has_kind(VALUE_COLOR, |value| COLORS.binary_search(&value).is_ok()) || root.values.binary_search(&value).is_ok();
    get_theme_value_utility(root.name, value, project).or(is_named.then_some(root.default))
  })
}

/// Gets if the text is an arbitrary value (ex. `[3px]` or `(--my-variable)`).
fn is_arbitrary_value(text: &str) -> bool {
  text.len() > 2 && (text.starts_with('[') && text.ends_with(']') || text.starts_with('(') && text.ends_with(')'))
}

/// Gets if the text is a whole number the way Tailwind accepts one, which is without leading zeros.
fn is_integer(text: &str) -> bool {
  !text.is_empty() && text.bytes().all(|byte| byte.is_ascii_digit()) && (text.len() == 1 || !text.starts_with('0'))
}

/// Gets if the text is a number with a decimal that doesn't have trailing zeros.
fn is_decimal(text: &str) -> bool {
  text
    .split_once('.')
    .is_some_and(|(whole, decimal)| is_integer(whole) && !decimal.ends_with('0') && is_integer(decimal.trim_start_matches('0')))
}

/// Gets if the text is a number with a decimal that's a multiple of 0.25.
fn is_quarter(text: &str) -> bool {
  text
    .split_once('.')
    .is_some_and(|(whole, decimal)| is_integer(whole) && matches!(decimal, "25" | "5" | "75"))
}

fn is_fraction(text: &str) -> bool {
  text
    .split_once('/')
    .is_some_and(|(numerator, denominator)| is_integer(numerator) && is_integer(denominator))
}

fn is_percentage(text: &str) -> bool {
  text.strip_suffix('%').is_some_and(is_integer)
}

/// Gets if the modifier may follow a utility that has the provided kind of modifier.
fn is_valid_modifier(modifier: &str, kind: Modifier, project: &Project) -> bool {
  let is_number = || is_arbitrary_value(modifier) || is_integer(modifier) || is_quarter(modifier);
  match kind {
    Modifier::None => false,
    Modifier::Arbitrary => is_arbitrary_value(modifier),
    Modifier::Opacity => is_number(),
    Modifier::LineHeight => is_number() || LINE_HEIGHTS.binary_search(&modifier).is_ok() || project.theme_value("--leading-", modifier).is_some(),
    Modifier::Any => !modifier.is_empty(),
  }
}

/// Gets the position of a utility that the project defines.
fn get_custom_utility_position(utility: &str, project: &Project) -> Option<u16> {
  if project.utilities.is_empty() {
    return None;
  }
  let properties = project.utilities.get(utility).or_else(|| {
    // ones that have a value are named like `tab-*`
    roots_and_values(utility).find_map(|(root, _)| {
      let mut names_and_properties = project.utilities.range::<str, _>((Bound::Included(root), Bound::Unbounded));
      names_and_properties.find_map(|(name, properties)| (name.strip_prefix(root)? == "-*").then_some(properties))
    })
  })?;
  Some(get_properties_position(properties.iter().map(|property| property.as_str()), properties.len()))
}

/// Gets the position and modifier of a utility whose value is the name of
/// one of the project's theme variables (ex. `text-huge` for `--text-huge`).
fn get_theme_value_utility(root: &str, value: &str, project: &Project) -> Option<(u16, Modifier)> {
  if project.theme.is_empty() {
    return None;
  }
  UTILITY_THEME_VALUES
    .iter()
    .find(|(entry_root, namespace, _, _)| *entry_root == root && project.theme_value(namespace, value).is_some())
    .map(|entry| (entry.2, entry.3))
}

/// Gets the position of a utility that sets the provided CSS properties.
fn get_properties_position<'a>(properties: impl ExactSizeIterator<Item = &'a str>, declaration_count: usize) -> u16 {
  // the indexes of the properties in Tailwind's property order from lowest to highest,
  // which only needs to allocate for a utility that sets a lot of properties
  let mut small_order = [0; 32];
  let mut large_order;
  let order: &mut [u16] = if properties.len() <= small_order.len() {
    &mut small_order
  } else {
    large_order = vec![0; properties.len()];
    &mut large_order
  };
  let mut len = 0;
  for index in properties
    .filter_map(|property| find(PROPERTIES, property, |entry| entry.0))
    .map(|entry| entry.1)
  {
    let insert_index = order[..len].partition_point(|other| *other < index);
    let is_new = insert_index == len || order[insert_index] != index;
    if is_new {
      order.copy_within(insert_index..len, insert_index + 1);
      order[insert_index] = index;
      len += 1;
    }
  }
  let order = &order[..len];
  let result = SORT_KEYS.binary_search_by(|(other_order, other_count)| {
    // orders are compared by their first difference, where one that ends sooner goes after
    let other_order_then_end = other_order.iter().map(Some).chain(std::iter::once(None));
    let order_then_end = order.iter().map(Some).chain(std::iter::once(None));
    let ordering = other_order_then_end
      .zip(order_then_end)
      .map(|(other_index, index)| match (other_index, index) {
        (Some(other_index), Some(index)) => other_index.cmp(index),
        (None, Some(_)) => Ordering::Greater,
        (Some(_), None) => Ordering::Less,
        (None, None) => Ordering::Equal,
      })
      .find(|ordering| *ordering != Ordering::Equal)
      .unwrap_or(Ordering::Equal);
    // then the one with more declarations goes first
    ordering.then_with(|| declaration_count.cmp(&(*other_count as usize)))
  });
  match result {
    Ok(index) => index as u16 * 2 + 1,
    Err(index) => index as u16 * 2,
  }
}

enum ArbitraryValueKind {
  Length,
  Color,
}

/// Guesses what an arbitrary value is for the roots that are for more than one
/// CSS property (ex. `text-[14px]` sets the font size and `text-[#fff]` the color).
fn get_arbitrary_value_kind(value: &str) -> Option<ArbitraryValueKind> {
  let is_variable = value.starts_with('(');
  let value = value.strip_prefix(['[', '('])?;
  if let Some((data_type, _)) = value.split_once(':') {
    return match data_type {
      "length" | "percentage" | "number" | "absolute-size" | "relative-size" => Some(ArbitraryValueKind::Length),
      "color" => Some(ArbitraryValueKind::Color),
      _ => None,
    };
  }
  let is_number = value.trim_start_matches(['-', '+', '.']).starts_with(|c: char| c.is_ascii_digit());
  if is_variable {
    None
  } else if is_number || value.starts_with("calc(") {
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

impl<'a> Project<'a> {
  /// Gets the value of the theme variable with the provided prefix and name
  /// (ex. `--breakpoint-` and `3xl` for `--breakpoint-3xl`).
  fn theme_value(&self, namespace: &str, name: &str) -> Option<&'a str> {
    self
      .theme
      .range::<str, _>((Bound::Included(namespace), Bound::Unbounded))
      .take_while(|(key, _)| key.starts_with(namespace))
      .find(|(key, _)| &key[namespace.len()..] == name)
      .map(|(_, value)| value.as_str())
  }
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
  /// Adds a variant, returning `None` when the variant isn't known or there are too many.
  fn insert(&mut self, variant: &'a str, project: &Project) -> Option<()> {
    if !is_known_variant(variant, project) {
      return None;
    }
    let mut index = 0;
    while index < self.len {
      match compare_variants(variant, self.items[index], project) {
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

  fn cmp(&self, other: &ClassVariants, project: &Project) -> Ordering {
    for (a, b) in self.items[..self.len].iter().zip(&other.items[..other.len]) {
      let ordering = compare_variants(a, b, project);
      if ordering != Ordering::Equal {
        return ordering;
      }
    }
    self.len.cmp(&other.len)
  }
}

fn is_known_variant(variant: &str, project: &Project) -> bool {
  if variant.starts_with('[') {
    return is_arbitrary_value(variant);
  }
  let variant = VariantInfo::parse(variant, project);
  if variant.modifier.is_some() && !variant.has_modifier {
    return false;
  }
  match (variant.kind, variant.value) {
    (Some(VariantKind::Static), _) => true,
    (Some(VariantKind::Compound), Some(value)) => is_known_variant(value, project),
    (Some(VariantKind::Functional), Some(value)) => {
      is_arbitrary_value(value)
        || match variant.values {
          VariantValues::Any => true,
          VariantValues::Integer => is_integer(value),
          VariantValues::Known => variant.breakpoint_value(project).is_some(),
        }
    }
    _ => false,
  }
}

/// Compares the same way as Tailwind's `Variants#compare`.
fn compare_variants(a: &str, b: &str, project: &Project) -> Ordering {
  // arbitrary variants (ex. `[&>*]`) go last
  match (a.starts_with('['), b.starts_with('[')) {
    (true, true) => return a.cmp(b),
    (true, false) => return Ordering::Greater,
    (false, true) => return Ordering::Less,
    (false, false) => {}
  }

  let (a, b) = (VariantInfo::parse(a, project), VariantInfo::parse(b, project));
  let ordering = a.order.cmp(&b.order);
  if ordering != Ordering::Equal {
    return ordering;
  }
  if a.kind == Some(VariantKind::Compound) && b.kind == Some(VariantKind::Compound) {
    return compare_variants(a.value.unwrap_or(""), b.value.unwrap_or(""), project).then_with(|| a.modifier.cmp(&b.modifier));
  }
  if a.value_order != VariantValueOrder::None {
    let is_ascending = a.value_order == VariantValueOrder::Ascending;
    let ordering = match (a.breakpoint_value(project), b.breakpoint_value(project)) {
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

/// The order of the first of a project's custom variants, which go after Tailwind's in the order they're defined.
const CUSTOM_VARIANT_ORDER_START: u16 = 10_000;
/// The order of variants that aren't known, which classes with a known utility never have.
const UNKNOWN_VARIANT_ORDER: u16 = u16::MAX;

struct VariantInfo<'a> {
  /// The text of the variant without its modifier.
  text: &'a str,
  root: &'a str,
  value: Option<&'a str>,
  modifier: Option<&'a str>,
  /// The kind of variant or `None` when the variant isn't known.
  kind: Option<VariantKind>,
  order: u16,
  value_order: VariantValueOrder,
  /// What the value may be.
  values: VariantValues,
  /// If the variant may have a modifier.
  has_modifier: bool,
}

impl<'a> VariantInfo<'a> {
  fn parse(variant: &'a str, project: &Project) -> VariantInfo<'a> {
    let (text, modifier) = split_modifier(variant);
    let found = find(VARIANTS, text, |entry| entry.0).map(|entry| (entry, None)).or_else(|| {
      // container query variants don't have a dash before their value (ex. `@md`)
      let container_value = text.strip_prefix('@').filter(|value| !value.starts_with("max-") && !value.starts_with("min-"));
      let mut roots_and_values = roots_and_values(text).chain(container_value.map(|value| ("@", value)));
      roots_and_values.find_map(|(root, value)| {
        let entry = find(VARIANTS, root, |entry| entry.0).filter(|entry| entry.1 != VariantKind::Static)?;
        Some((entry, Some(value)))
      })
    });
    let unknown = VariantInfo {
      text,
      root: text,
      value: None,
      modifier,
      kind: None,
      order: UNKNOWN_VARIANT_ORDER,
      value_order: VariantValueOrder::None,
      values: VariantValues::Known,
      has_modifier: false,
    };
    if let Some(((root, kind, order, value_order, values, has_modifier), value)) = found {
      VariantInfo {
        root,
        value,
        kind: Some(*kind),
        order: *order,
        value_order: *value_order,
        values: *values,
        has_modifier: *has_modifier,
        ..unknown
      }
    } else if let Some(index) = project.variants.iter().position(|name| name == text) {
      VariantInfo {
        kind: Some(VariantKind::Static),
        order: CUSTOM_VARIANT_ORDER_START + index as u16,
        ..unknown
      }
    } else if project.theme_value("--breakpoint-", text).is_some() {
      // a breakpoint the project adds is ordered with Tailwind's
      let breakpoint = find(VARIANTS, "sm", |entry| entry.0);
      VariantInfo {
        kind: breakpoint.map(|entry| entry.1),
        order: breakpoint.map(|entry| entry.2).unwrap_or(UNKNOWN_VARIANT_ORDER),
        value_order: breakpoint.map(|entry| entry.3).unwrap_or(VariantValueOrder::None),
        ..unknown
      }
    } else {
      unknown
    }
  }

  /// Gets the value of a variant that's ordered by its value (ex. `48rem` for `md` or `min-[48rem]`).
  fn breakpoint_value<'b>(&'b self, project: &Project<'b>) -> Option<&'b str> {
    if let Some(value) = self.value.and_then(|value| value.strip_prefix('[')) {
      return value.strip_suffix(']').filter(|value| !value.contains("var("));
    }
    let namespace = if self.root.starts_with('@') { "--container-" } else { "--breakpoint-" };
    project
      .theme_value(namespace, self.value.unwrap_or(self.text))
      .or_else(|| find(VARIANT_VALUES, self.text, |entry| entry.0).map(|entry| entry.1))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn tables_are_sorted_for_searching() {
    assert!(UTILITIES.is_sorted_by(|a, b| a.0 < b.0));
    assert!(UTILITY_ROOTS.is_sorted_by(|a, b| a.name < b.name));
    assert!(UTILITY_ROOTS.iter().all(|root| root.values.is_sorted_by(|a, b| a < b)));
    assert!(COLORS.is_sorted_by(|a, b| a < b));
    assert!(LINE_HEIGHTS.is_sorted_by(|a, b| a < b));
    assert!(PROPERTIES.is_sorted_by(|a, b| a.0 < b.0));
    assert!(VARIANTS.is_sorted_by(|a, b| a.0 < b.0));
    assert!(VARIANT_VALUES.is_sorted_by(|a, b| a.0 < b.0));
  }

  #[test]
  fn sorts_the_same_as_tailwind() {
    assert_sorts_the_same_as_tailwind(include_str!("sort_tests.txt"), &Project::default());
  }

  #[test]
  fn sorts_the_same_as_tailwind_for_a_project() {
    // this is what the project that the file is for has in its CSS
    let theme = [
      ("--breakpoint-3xl", "120rem"),
      ("--breakpoint-xs", "30rem"),
      ("--container-8xl", "90rem"),
      ("--color-brand", "#f00"),
      ("--color-brand-dark", "#900"),
      ("--text-huge", "4rem"),
      ("--font-display", "\"Display\""),
      ("--font-weight-heavy", "950"),
      ("--shadow-glow", "0 0 8px #fff"),
      ("--radius-blob", "3rem"),
      ("--spacing-gutter", "1.5rem"),
      ("--ease-snappy", "cubic-bezier(0.2, 0, 0, 1)"),
    ];
    let theme = theme.into_iter().map(|(name, value)| (name.to_string(), value.to_string())).collect();
    let variants = ["theme-midnight".to_string(), "hocus".to_string()];
    let utilities = [("btn", vec!["display", "padding"]), ("tab-*", vec!["tab-size"])];
    let utilities = utilities
      .into_iter()
      .map(|(name, properties)| (name.to_string(), properties.into_iter().map(|property| property.to_string()).collect()))
      .collect();
    let project = Project {
      prefix: Some("tw"),
      theme: &theme,
      variants: &variants,
      utilities: &utilities,
    };
    assert_sorts_the_same_as_tailwind(include_str!("sort_tests_project.txt"), &project);
  }

  #[track_caller]
  fn assert_sorts_the_same_as_tailwind(tests: &str, project: &Project) {
    let mut failures = Vec::new();
    let mut count = 0;
    for line in tests.lines().filter(|line| !line.starts_with('#')) {
      let (text, expected) = line.split_once(" => ").unwrap();
      let actual = sort_class_names(text, &Default::default(), project);
      count += 1;
      if actual != expected {
        failures.push(format!("   input: {}\nexpected: {}\n  actual: {}", text, expected, actual));
      }
    }
    assert!(count > 0);
    assert!(
      failures.is_empty(),
      "{} of {} failed:\n\n{}",
      failures.len(),
      count,
      failures[..failures.len().min(10)].join("\n\n")
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
  fn treats_classes_with_unknown_variants_as_unknown() {
    assert_sorts("p-4 custom:flex", "custom:flex p-4");
    assert_sorts("m-2 small:p-4 hover:flex hoverable:p-4", "small:p-4 hoverable:p-4 m-2 hover:flex");
    assert_sorts("m-2 group-custom:p-4 not-hover:p-4", "group-custom:p-4 m-2 not-hover:p-4");
  }

  #[test]
  fn treats_classes_with_values_tailwind_does_not_have_as_unknown() {
    assert_sorts("flex p-banana", "p-banana flex");
    assert_sorts("flex p-1.3 p-1.5", "p-1.3 flex p-1.5");
    assert_sorts("flex -p-4 -m-4", "-p-4 -m-4 flex");
    assert_sorts("flex p-4/foo", "p-4/foo flex");
    assert_sorts("flex p-4/50", "p-4/50 flex");
    assert_sorts("flex bg-red-500/foo bg-red-500/50", "bg-red-500/foo flex bg-red-500/50");
    assert_sorts("flex text-sm/foo text-sm/tight", "text-sm/foo flex text-sm/tight");
    assert_sorts("p-4 max-foo:flex max-md:flex", "max-foo:flex p-4 max-md:flex");
    assert_sorts("p-4 nth-foo:flex nth-3:flex", "nth-foo:flex p-4 nth-3:flex");
    assert_sorts("p-4 hover/foo:flex group-hover/foo:flex", "hover/foo:flex p-4 group-hover/foo:flex");
    assert_sorts("p-4 hover::flex :flex hover:flex", "hover::flex :flex p-4 hover:flex");
    assert_sorts("p-4 [&]oops:flex [&]:flex", "[&]oops:flex p-4 [&]:flex");
  }

  #[test]
  fn sorts_important_utilities_with_their_utility() {
    assert_sorts("flex !p-4 m-2", "m-2 flex !p-4");
    assert_sorts("flex p-4! m-2", "m-2 flex p-4!");
    assert_sorts("hover:!p-4 !flex", "!flex hover:!p-4");
  }

  #[test]
  fn preserves_whitespace() {
    let options = SortOptions {
      preserve_whitespace: true,
      ..Default::default()
    };
    let sort = |text| sort_class_names(text, &options, &Project::default());
    assert_eq!(sort("  sm:bg-black   bg-red-500  "), "  bg-red-500   sm:bg-black  ");
    assert_eq!(sort("sm:p-0\n   p-0"), "p-0\n   sm:p-0");
    assert_eq!(sort("sm:p-0  p-0 \t p-0\nfoo"), "foo  p-0\nsm:p-0");
    assert_eq!(sort("  "), "  ");
    assert!(matches!(sort(" p-0  sm:p-0 "), Cow::Borrowed(_)));
    let options = SortOptions {
      ignore_first: true,
      ignore_last: true,
      ..options
    };
    assert_eq!(sort_class_names("z  sm:p-0 \t p-0\na", &options, &Project::default()), "z  p-0 \t sm:p-0\na");
  }

  #[test]
  fn preserves_duplicates() {
    let options = SortOptions {
      preserve_duplicates: true,
      ..Default::default()
    };
    let sort = |text| sort_class_names(text, &options, &Project::default());
    assert_eq!(sort("bg-red-500 sm:bg-black bg-red-500"), "bg-red-500 bg-red-500 sm:bg-black");
    assert_eq!(sort("sm:p-0 p-0 p-0"), "p-0 p-0 sm:p-0");
    assert!(matches!(sort("p-0 p-0 sm:p-0"), Cow::Borrowed(_)));
  }

  #[test]
  fn borrows_when_already_sorted() {
    for text in ["", " ", "foo", "foo bar foo", "flex p-4", "foo bar p-4 px-2 hover:px-2 ...", "{{ 'p-4 flex' }}"] {
      assert!(
        matches!(sort_class_names(text, &Default::default(), &Project::default()), Cow::Borrowed(result) if result == text),
        "{}",
        text
      );
    }
    let options = SortOptions {
      collapse_start: false,
      collapse_end: false,
      ..Default::default()
    };
    assert!(matches!(sort_class_names(" flex p-4 ", &options, &Project::default()), Cow::Borrowed(_)));
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
  fn sorts_custom_utilities_with_many_properties() {
    // this is where Tailwind puts a utility that sets these properties
    let properties = [
      "text-transform",
      "font-style",
      "font-stretch",
      "font-variant-numeric",
      "text-decoration-line",
      "text-decoration-color",
      "text-decoration-style",
      "text-decoration-thickness",
      "text-underline-offset",
      "-webkit-font-smoothing",
      "caret-color",
      "accent-color",
      "color-scheme",
      "opacity",
      "background-blend-mode",
      "mix-blend-mode",
      "box-shadow",
      "outline",
      "outline-width",
      "outline-offset",
      "outline-color",
      "filter",
      "backdrop-filter",
      "transition-property",
      "transition-behavior",
      "transition-delay",
      "transition-duration",
      "transition-timing-function",
      "will-change",
      "contain",
      "content",
      "forced-color-adjust",
      "display",
    ];
    let utilities = BTreeMap::from([("big".to_string(), properties.iter().map(|property| property.to_string()).collect())]);
    let project = Project {
      utilities: &utilities,
      ..Default::default()
    };
    let actual = sort_class_names("p-4 big flex opacity-50 m-2", &Default::default(), &project);
    assert_eq!(actual, "m-2 big flex p-4 opacity-50");
  }

  #[test]
  fn handles_quotes_and_escapes_in_arbitrary_values() {
    // these are what Tailwind sorts them to
    assert_sorts("p-4 before:content-[')'] flex", "flex p-4 before:content-[')']");
    assert_sorts("p-4 content-[']'] flex", "flex p-4 content-[']']");
    assert_sorts("p-4 content-['['] flex", "flex p-4 content-['[']");
    assert_sorts("p-4 content-['a:b'] flex", "flex p-4 content-['a:b']");
    assert_sorts("p-4 [&:is(a,'b:c')]:flex hover:flex m-2", "m-2 p-4 hover:flex [&:is(a,'b:c')]:flex");
    assert_sorts("p-4 bg-[url('a/b.png')] flex", "flex bg-[url('a/b.png')] p-4");
    assert_sorts("p-4 content-[\\]] flex", "flex p-4 content-[\\]]");
    assert_sorts("p-4 content-[]] flex", "content-[]] flex p-4");
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
      ..Default::default()
    };
    assert_eq!(
      sort_class_names("sm:block inline flex", &ignore_last, &Project::default()),
      "inline sm:block flex"
    );
    assert_eq!(
      sort_class_names("sm:block md:inline flex", &ignore_first, &Project::default()),
      "sm:block flex md:inline"
    );
    assert_eq!(sort_class_names("   flex  flex flex", &ignore_last, &Project::default()), "flex flex");
    assert_eq!(sort_class_names("block block", &ignore_first, &Project::default()), "block block");
    assert_eq!(sort_class_names("a sm:p-0 p-0 b", &ignore_both, &Project::default()), "a p-0 sm:p-0 b");
    assert_eq!(sort_class_names("a", &ignore_both, &Project::default()), "a");
    assert_eq!(sort_class_names("a b", &ignore_both, &Project::default()), "a b");
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
    assert_eq!(sort_class_names("sm:p-0 p-0  ", &keep_end, &Project::default()), "p-0 sm:p-0 ");
    assert_eq!(sort_class_names("  sm:p-0 p-0  ", &keep_end, &Project::default()), "p-0 sm:p-0 ");
    assert_eq!(sort_class_names("  sm:p-0 p-0  ", &keep_start, &Project::default()), " p-0 sm:p-0");
    assert_eq!(
      sort_class_names(" aspect-square w-full", &keep_start, &Project::default()),
      " aspect-square w-full"
    );
    assert_eq!(
      sort_class_names(" min-h-0 grow basis-0", &keep_start, &Project::default()),
      " min-h-0 grow basis-0"
    );
    assert_eq!(sort_class_names("flex ", &keep_end, &Project::default()), "flex ");
  }

  #[track_caller]
  fn assert_sorts(text: &str, expected: &str) {
    // the tests from Prettier's plugin are for a project that has this color
    let theme = BTreeMap::from([("--color-tomato".to_string(), "tomato".to_string())]);
    let project = Project {
      theme: &theme,
      ..Default::default()
    };
    assert_eq!(sort_class_names(text, &Default::default(), &project), expected);
  }
}
