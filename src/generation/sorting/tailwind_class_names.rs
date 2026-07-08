use std::cmp::Ordering;

const COMPONENTS_LAYER_CLASSES: &[&str] = &["container$"];

const UTILITIES_LAYER_CLASSES: &[&str] = &[
  "sr-only$",
  "not-sr-only$",
  "pointer-events-none$",
  "pointer-events-auto$",
  "visible$",
  "invisible$",
  "collapse$",
  "static$",
  "fixed$",
  "absolute$",
  "relative$",
  "sticky$",
  "inset-",
  "inset-x-",
  "inset-y-",
  "start-",
  "end-",
  "top-",
  "right-",
  "bottom-",
  "left-",
  "isolate$",
  "isolation-auto$",
  "z-",
  "order-",
  "col-",
  "col-start-",
  "col-end-",
  "row-",
  "row-start-",
  "row-end-",
  "float-start$",
  "float-end$",
  "float-right$",
  "float-left$",
  "float-none$",
  "clear-start$",
  "clear-end$",
  "clear-left$",
  "clear-right$",
  "clear-both$",
  "clear-none$",
  "m-",
  "mx-",
  "my-",
  "ms-",
  "me-",
  "mt-",
  "mr-",
  "mb-",
  "ml-",
  "box-border$",
  "box-content$",
  "line-clamp-",
  "line-clamp-none$",
  "block$",
  "inline-block$",
  "inline$",
  "flex$",
  "inline-flex$",
  "table$",
  "inline-table$",
  "table-caption$",
  "table-cell$",
  "table-column$",
  "table-column-group$",
  "table-footer-group$",
  "table-header-group$",
  "table-row-group$",
  "table-row$",
  "flow-root$",
  "grid$",
  "inline-grid$",
  "contents$",
  "list-item$",
  "hidden$",
  "aspect-",
  "size-",
  "h-",
  "max-h-",
  "min-h-",
  "w-",
  "min-w-",
  "max-w-",
  "flex-",
  "flex-shrink$",
  "flex-shrink-",
  "shrink$",
  "shrink-",
  "flex-grow$",
  "flex-grow-",
  "grow$",
  "grow-",
  "basis-",
  "table-auto$",
  "table-fixed$",
  "caption-top$",
  "caption-bottom$",
  "border-collapse$",
  "border-separate$",
  "border-spacing-",
  "border-spacing-x-",
  "border-spacing-y-",
  "origin-",
  "translate-x-",
  "translate-y-",
  "rotate-",
  "skew-x-",
  "skew-y-",
  "scale-",
  "scale-x-",
  "scale-y-",
  "transform$",
  "transform-cpu$",
  "transform-gpu$",
  "transform-none$",
  "animate-",
  "cursor-",
  "touch-auto$",
  "touch-none$",
  "touch-pan-x$",
  "touch-pan-left$",
  "touch-pan-right$",
  "touch-pan-y$",
  "touch-pan-up$",
  "touch-pan-down$",
  "touch-pinch-zoom$",
  "touch-manipulation$",
  "select-none$",
  "select-text$",
  "select-all$",
  "select-auto$",
  "resize-none$",
  "resize-y$",
  "resize-x$",
  "resize$",
  "snap-none$",
  "snap-x$",
  "snap-y$",
  "snap-both$",
  "snap-mandatory$",
  "snap-proximity$",
  "snap-start$",
  "snap-end$",
  "snap-center$",
  "snap-align-none$",
  "snap-normal$",
  "snap-always$",
  "scroll-m-",
  "scroll-mx-",
  "scroll-my-",
  "scroll-ms-",
  "scroll-me-",
  "scroll-mt-",
  "scroll-mr-",
  "scroll-mb-",
  "scroll-ml-",
  "scroll-p-",
  "scroll-px-",
  "scroll-py-",
  "scroll-ps-",
  "scroll-pe-",
  "scroll-pt-",
  "scroll-pr-",
  "scroll-pb-",
  "scroll-pl-",
  "list-inside$",
  "list-outside$",
  "list-",
  "list-image-",
  "appearance-none$",
  "appearance-auto$",
  "columns-",
  "break-before-",
  "break-inside-",
  "break-after-",
  "auto-cols-",
  "grid-flow-row$",
  "grid-flow-col$",
  "grid-flow-dense$",
  "grid-flow-row-dense$",
  "grid-flow-col-dense$",
  "auto-rows-",
  "grid-cols-",
  "grid-rows-",
  "flex-row$",
  "flex-row-reverse$",
  "flex-col$",
  "flex-col-reverse$",
  "flex-wrap$",
  "flex-wrap-reverse$",
  "flex-nowrap$",
  "place-content-",
  "place-items-",
  "content-",
  "items-",
  "justify-normal$",
  "justify-start$",
  "justify-end$",
  "justify-center$",
  "justify-between$",
  "justify-around$",
  "justify-evenly$",
  "justify-stretch$",
  "justify-items-",
  "gap-",
  "gap-x-",
  "gap-y-",
  "space-x-",
  "space-y-",
  "space-y-reverse$",
  "space-x-reverse$",
  "divide-x$",
  "divide-x-",
  "divide-y$",
  "divide-y-",
  "divide-y-reverse$",
  "divide-x-reverse$",
  "divide-solid$",
  "divide-dashed$",
  "divide-dotted$",
  "divide-double$",
  "divide-none$",
  "divide-",
  "divide-opacity-",
  "place-self-",
  "self-",
  "justify-self-",
  "overflow-auto$",
  "overflow-hidden$",
  "overflow-clip$",
  "overflow-visible$",
  "overflow-scroll$",
  "overflow-x-",
  "overflow-y-",
  "overscroll-",
  "overscroll-y-",
  "overscroll-x-",
  "scroll-auto$",
  "scroll-smooth$",
  "truncate$",
  "overflow-ellipsis$",
  "text-ellipsis$",
  "text-clip$",
  "hyphens-",
  "whitespace-",
  "text-wrap$",
  "text-nowrap$",
  "text-balance$",
  "text-pretty$",
  "break-normal$",
  "break-words$",
  "break-all$",
  "break-keep$",
  "rounded$",
  "rounded-",
  "rounded-s$",
  "rounded-s-",
  "rounded-e$",
  "rounded-e-",
  "rounded-t$",
  "rounded-t-",
  "rounded-r$",
  "rounded-r-",
  "rounded-b$",
  "rounded-b-",
  "rounded-l$",
  "rounded-l-",
  "rounded-ss$",
  "rounded-ss-",
  "rounded-se$",
  "rounded-se-",
  "rounded-ee$",
  "rounded-ee-",
  "rounded-es$",
  "rounded-es-",
  "rounded-tl$",
  "rounded-tl-",
  "rounded-tr$",
  "rounded-tr-",
  "rounded-br$",
  "rounded-br-",
  "rounded-bl$",
  "rounded-bl-",
  "border$",
  "border-",
  "border-x$",
  "border-x-",
  "border-y$",
  "border-y-",
  "border-s$",
  "border-s-",
  "border-e$",
  "border-e-",
  "border-t$",
  "border-t-",
  "border-r$",
  "border-r-",
  "border-b$",
  "border-b-",
  "border-l$",
  "border-l-",
  "border-solid$",
  "border-dashed$",
  "border-dotted$",
  "border-double$",
  "border-hidden$",
  "border-none$",
  "border-opacity-",
  "bg-",
  "bg-opacity-",
  "from-",
  "via-",
  "to-",
  "decoration-slice$",
  "decoration-clone$",
  "box-decoration-slice$",
  "box-decoration-clone$",
  "bg-fixed$",
  "bg-local$",
  "bg-scroll$",
  "bg-clip-",
  "bg-repeat$",
  "bg-no-repeat$",
  "bg-repeat-x$",
  "bg-repeat-y$",
  "bg-repeat-round$",
  "bg-repeat-space$",
  "bg-origin-",
  "fill-",
  "stroke-",
  "object-contain$",
  "object-cover$",
  "object-fill$",
  "object-none$",
  "object-scale-down$",
  "object-",
  "p-",
  "px-",
  "py-",
  "ps-",
  "pe-",
  "pt-",
  "pr-",
  "pb-",
  "pl-",
  "text-left$",
  "text-center$",
  "text-right$",
  "text-justify$",
  "text-start$",
  "text-end$",
  "indent-",
  "align-baseline$",
  "align-top$",
  "align-middle$",
  "align-bottom$",
  "align-text-top$",
  "align-text-bottom$",
  "align-sub$",
  "align-super$",
  "align-",
  "font-",
  "text-",
  "uppercase$",
  "lowercase$",
  "capitalize$",
  "normal-case$",
  "italic$",
  "not-italic$",
  "normal-nums$",
  "ordinal$",
  "slashed-zero$",
  "lining-nums$",
  "oldstyle-nums$",
  "proportional-nums$",
  "tabular-nums$",
  "diagonal-fractions$",
  "stacked-fractions$",
  "leading-",
  "tracking-",
  "text-opacity-",
  "underline$",
  "overline$",
  "line-through$",
  "no-underline$",
  "decoration-",
  "decoration-solid$",
  "decoration-double$",
  "decoration-dotted$",
  "decoration-dashed$",
  "decoration-wavy$",
  "underline-offset-",
  "antialiased$",
  "subpixel-antialiased$",
  "placeholder-",
  "placeholder-opacity-",
  "caret-",
  "accent-",
  "opacity-",
  "bg-blend-",
  "mix-blend-",
  "shadow$",
  "shadow-",
  "outline-none$",
  "outline$",
  "outline-dashed$",
  "outline-dotted$",
  "outline-double$",
  "outline-",
  "outline-offset-",
  "ring$",
  "ring-",
  "ring-inset$",
  "ring-opacity-",
  "ring-offset-",
  "blur$",
  "blur-",
  "brightness-",
  "contrast-",
  "drop-shadow$",
  "drop-shadow-",
  "grayscale$",
  "grayscale-",
  "hue-rotate-",
  "invert$",
  "invert-",
  "saturate-",
  "sepia$",
  "sepia-",
  "filter$",
  "filter-none$",
  "backdrop-blur$",
  "backdrop-blur-",
  "backdrop-brightness-",
  "backdrop-contrast-",
  "backdrop-grayscale$",
  "backdrop-grayscale-",
  "backdrop-hue-rotate-",
  "backdrop-invert$",
  "backdrop-invert-",
  "backdrop-opacity-",
  "backdrop-saturate-",
  "backdrop-sepia$",
  "backdrop-sepia-",
  "backdrop-filter$",
  "backdrop-filter-none$",
  "transition$",
  "transition-",
  "delay-",
  "duration-",
  "ease-",
  "will-change-",
  "contain-",
  "content-",
  "forced-color-adjust-auto$",
  "forced-color-adjust-none$",
];

const VARIANT_CLASSES: &[&str] = &[
  "*",
  "first-letter",
  "first-line",
  "marker",
  "selection",
  "file",
  "placeholder",
  "backdrop",
  "before",
  "after",
  "first",
  "last",
  "only",
  "odd",
  "even",
  "first-of-type",
  "last-of-type",
  "only-of-type",
  "visited",
  "target",
  "open",
  "default",
  "checked",
  "indeterminate",
  "placeholder-shown",
  "autofill",
  "optional",
  "required",
  "valid",
  "invalid",
  "in-range",
  "out-of-range",
  "read-only",
  "empty",
  "focus-within",
  "hover",
  "focus",
  "focus-visible",
  "active",
  "enabled",
  "disabled",
  "group-hover",
  "group-focus",
  "group-active",
  "group",
  "peer-hover",
  "peer-focus",
  "peer-active",
  "peer",
  "has",
  "group-has",
  "peer-has",
  "aria",
  "group-aria",
  "peer-aria",
  "data",
  "group-data",
  "peer-data",
  "supports",
  "motion-safe",
  "motion-reduce",
  "contrast-more",
  "contrast-less",
  "max-sm",
  "max-md",
  "max-lg",
  "max-xl",
  "max-2xl",
  "max",
  "sm",
  "md",
  "lg",
  "xl",
  "2xl",
  "min",
  "portrait",
  "landscape",
  "ltr",
  "rtl",
  "dark",
  "forced-colors",
  "print",
];

pub fn sort_tailwind_class_names(class_names: &str) -> String {
  let classes = class_names.split_whitespace().fold(Vec::new(), |mut classes, class_name| {
    if !classes.contains(&class_name) {
      classes.push(class_name);
    }
    classes
  });
  let mut custom_classes = Vec::new();
  let mut tailwind_classes = Vec::new();

  for class_name in classes {
    match ClassInfo::from_class_name(class_name) {
      Some(class_info) => tailwind_classes.push(class_info),
      None => custom_classes.push(class_name),
    }
  }

  tailwind_classes.sort_by(|a, b| a.text.cmp(b.text));
  tailwind_classes.sort_by(compare_classes);

  custom_classes.extend(tailwind_classes.into_iter().map(|class_info| class_info.text));
  custom_classes.join(" ")
}

fn compare_classes(a: &ClassInfo, b: &ClassInfo) -> Ordering {
  cmp_optional(a.has_arbitrary_variants(), b.has_arbitrary_variants(), Ordering::Greater, Ordering::Less)
    .then_with(|| a.arbitrary_variants.len().cmp(&b.arbitrary_variants.len()))
    .then_with(|| a.arbitrary_variants.cmp(&b.arbitrary_variants))
    .then_with(|| cmp_optional(a.variant_weight.is_some(), b.variant_weight.is_some(), Ordering::Greater, Ordering::Less))
    .then_with(|| a.layer_index.cmp(&b.layer_index))
    .then_with(|| compare_variant_weights(a.variant_weight.as_deref(), b.variant_weight.as_deref()))
    .then_with(|| a.utility_index.cmp(&b.utility_index))
    .then_with(|| a.text.cmp(b.text))
}

fn cmp_optional(a: bool, b: bool, a_some_order: Ordering, b_some_order: Ordering) -> Ordering {
  match (a, b) {
    (true, true) | (false, false) => Ordering::Equal,
    (true, false) => a_some_order,
    (false, true) => b_some_order,
  }
}

fn compare_variant_weights(a: Option<&[usize]>, b: Option<&[usize]>) -> Ordering {
  match (a, b) {
    (Some(a), Some(b)) => a.len().cmp(&b.len()).then_with(|| a.cmp(b)),
    _ => Ordering::Equal,
  }
}

struct ClassInfo<'a> {
  text: &'a str,
  variant_weight: Option<Vec<usize>>,
  layer_index: usize,
  utility_index: usize,
  arbitrary_variants: Vec<&'a str>,
}

impl<'a> ClassInfo<'a> {
  fn from_class_name(class_name: &'a str) -> Option<ClassInfo<'a>> {
    let class_structure = tokenize_class(class_name)?;
    let utility_info = get_utility_info(&class_structure.utility)?;
    let mut arbitrary_variants = Vec::new();
    let mut variants = Vec::new();

    for variant in &class_structure.variants {
      if variant.arbitrary {
        arbitrary_variants.push(variant.text);
      } else {
        variants.push(*variant);
      }
    }

    Some(ClassInfo {
      text: class_name,
      variant_weight: compute_variant_weight(&variants),
      layer_index: utility_info.layer_index,
      utility_index: utility_info.utility_index,
      arbitrary_variants,
    })
  }

  fn has_arbitrary_variants(&self) -> bool {
    !self.arbitrary_variants.is_empty()
  }
}

#[derive(Clone, Copy)]
struct UtilityInfo {
  layer_index: usize,
  utility_index: usize,
}

fn get_utility_info(utility: &ClassSegment) -> Option<UtilityInfo> {
  if utility.arbitrary {
    return Some(UtilityInfo {
      layer_index: 2,
      utility_index: 0,
    });
  }

  get_utility_info_from_layer(0, COMPONENTS_LAYER_CLASSES, utility.text).or_else(|| get_utility_info_from_layer(1, UTILITIES_LAYER_CLASSES, utility.text))
}

fn get_utility_info_from_layer(layer_index: usize, layer_classes: &[&str], utility_text: &str) -> Option<UtilityInfo> {
  let mut longest_partial_match = None;
  let mut longest_partial_match_len = 0;

  for (utility_index, target) in layer_classes.iter().enumerate() {
    match matches_utility_target(target, utility_text) {
      UtilityMatch::Exact => return Some(UtilityInfo { layer_index, utility_index }),
      UtilityMatch::Partial => {
        if target.len() > longest_partial_match_len {
          longest_partial_match = Some(UtilityInfo { layer_index, utility_index });
          longest_partial_match_len = target.len();
        }
      }
      UtilityMatch::None => {}
    }
  }

  longest_partial_match
}

enum UtilityMatch {
  Exact,
  Partial,
  None,
}

fn matches_utility_target(target: &str, utility_text: &str) -> UtilityMatch {
  let target = target.strip_prefix('-').unwrap_or(target);
  let utility_text = utility_text.strip_prefix('-').unwrap_or(utility_text);
  if let Some(exact_target) = target.strip_suffix('$') {
    if utility_text == exact_target {
      UtilityMatch::Exact
    } else {
      UtilityMatch::None
    }
  } else if utility_text.starts_with(target) && utility_text != target {
    UtilityMatch::Partial
  } else {
    UtilityMatch::None
  }
}

fn compute_variant_weight(variants: &[ClassSegment]) -> Option<Vec<usize>> {
  let mut variant_weight = Vec::new();
  for variant in variants {
    let Some(variant_index) = find_variant_position(variant.text) else {
      continue;
    };
    if !variant_weight.contains(&variant_index) {
      variant_weight.push(variant_index);
    }
  }
  if variant_weight.is_empty() {
    None
  } else {
    variant_weight.sort_unstable();
    Some(variant_weight)
  }
}

fn find_variant_position(variant_text: &str) -> Option<usize> {
  let mut longest_partial_match = None;
  let mut longest_partial_match_len = 0;

  for (index, target) in VARIANT_CLASSES.iter().enumerate() {
    match matches_variant_target(target, variant_text) {
      VariantMatch::Exact => return Some(index),
      VariantMatch::Partial => {
        if target.len() > longest_partial_match_len {
          longest_partial_match = Some(index);
          longest_partial_match_len = target.len();
        }
      }
      VariantMatch::None => {}
    }
  }

  longest_partial_match
}

enum VariantMatch {
  Exact,
  Partial,
  None,
}

fn matches_variant_target(target: &str, variant_text: &str) -> VariantMatch {
  if target == variant_text || variant_text.strip_prefix(target).is_some_and(|rest| rest.starts_with("-[")) {
    VariantMatch::Exact
  } else if variant_text.starts_with(target) && variant_text != target {
    VariantMatch::Partial
  } else {
    VariantMatch::None
  }
}

#[derive(Clone, Copy)]
struct ClassSegment<'a> {
  arbitrary: bool,
  text: &'a str,
}

struct ClassStructure<'a> {
  variants: Vec<ClassSegment<'a>>,
  utility: ClassSegment<'a>,
}

fn tokenize_class(class_name: &str) -> Option<ClassStructure<'_>> {
  let mut arbitrary_block_depth = 0;
  let mut delimiter_indexes = Vec::new();

  for (index, byte) in class_name.bytes().enumerate() {
    match byte {
      b'[' => arbitrary_block_depth += 1,
      b']' => {
        if arbitrary_block_depth == 0 {
          return None;
        }
        arbitrary_block_depth -= 1;
      }
      b':' if arbitrary_block_depth == 0 => delimiter_indexes.push(index),
      _ => {}
    }
  }

  if arbitrary_block_depth != 0 {
    return None;
  }

  let mut segments = split_at_indexes(class_name, &delimiter_indexes)
    .into_iter()
    .map(|text| ClassSegment {
      arbitrary: text.starts_with('['),
      text,
    })
    .collect::<Vec<_>>();
  let utility = segments.pop()?;
  Some(ClassStructure { variants: segments, utility })
}

fn split_at_indexes<'a>(text: &'a str, indexes: &[usize]) -> Vec<&'a str> {
  let mut segments = Vec::new();
  let mut start_offset = 0;
  let mut start = 0;

  for &index in indexes {
    if index > start {
      segments.push(&text[start + start_offset..index]);
    }
    start_offset = 1;
    start = index;
  }

  if start + start_offset < text.len() {
    segments.push(&text[start + start_offset..]);
  }

  segments
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn should_sort_custom_classes_first() {
    assert_eq!(sort_tailwind_class_names("px-2 foo p-4 bar"), "foo bar p-4 px-2");
  }

  #[test]
  fn should_sort_variants_after_plain_utilities() {
    assert_eq!(
      sort_tailwind_class_names("hover:focus:m-2 foo hover:px-2 p-4"),
      "foo p-4 hover:px-2 hover:focus:m-2",
    );
  }

  #[test]
  fn should_handle_colons_in_arbitrary_segments() {
    assert_eq!(sort_tailwind_class_names("[&:hover]:p-4 flex"), "flex [&:hover]:p-4");
  }
}
