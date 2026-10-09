// Generates the tables in src/tailwind from Tailwind CSS:
//
//   deno run -A scripts/generate_tailwind_tables.ts
//
// - tables.rs has the order of Tailwind's utilities and variants
// - sort_tests.txt has class lists sorted by Tailwind, which the tests in
//   src/tailwind check the Rust code sorts the same way
// - sort_tests_project.txt has the same for a project that customizes Tailwind
//
// Bump the version in the import below to update to a newer Tailwind.
import { readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { __unstable__loadDesignSystem } from "npm:tailwindcss@4.3.3";

const tailwindVersion = "4.3.3";
const require = createRequire(import.meta.url);

type ModifierKind = "None" | "Arbitrary" | "Opacity" | "LineHeight" | "Any";

interface UtilityRoot {
  /**
   * Class names that have the position of most of the root's named values,
   * a number, an arbitrary length, and an arbitrary color.
   */
  default: string;
  number: string;
  arbitraryLength: string;
  arbitraryColor: string;
  /** Kinds of values that are valid without being listed, which are the names of the constants in tables.rs. */
  valueKinds: string[];
  /** Named values that are valid and aren't one of the kinds above. */
  values: string[];
}

interface SortKey {
  /** Indexes of the CSS properties a utility sets in Tailwind's property order. */
  order: number[];
  /** Number of declarations, where utilities with more go first. */
  count: number;
}

/** Prefixes of the theme variables along with a value to use for a theme variable that has it. */
const themeNamespaces: [string, string][] = [
  ["--color-", "#000"],
  ["--text-", "1rem"],
  ["--font-", "serif"],
  ["--font-weight-", "700"],
  ["--tracking-", "1em"],
  ["--leading-", "1"],
  ["--spacing-", "1rem"],
  ["--radius-", "1rem"],
  ["--shadow-", "0 0 #000"],
  ["--inset-shadow-", "inset 0 0 #000"],
  ["--drop-shadow-", "0 0 #000"],
  ["--text-shadow-", "0 0 #000"],
  ["--blur-", "1px"],
  ["--perspective-", "1px"],
  ["--aspect-", "1 / 1"],
  ["--ease-", "linear"],
  ["--animate-", "spin 1s linear"],
  ["--container-", "1rem"],
  ["--breakpoint-", "1rem"],
];
/** Kinds of values a root may accept without them being listed along with a value to check that with. */
const bareValues: [string, string][] = [
  ["INTEGER", "7"],
  ["QUARTER", "7.5"],
  ["DECIMAL", "7.3"],
  ["FRACTION", "7/9"],
  ["PERCENTAGE", "7%"],
];
const valueKinds: [string, string][] = [
  ["INTEGER", "A whole number (ex. `z-17`)."],
  ["QUARTER", "A number that's a multiple of 0.25 (ex. `p-1.5`)."],
  ["DECIMAL", "Any number with a decimal (ex. `opacity-2.3`)."],
  ["FRACTION", "A fraction (ex. `w-3/7`)."],
  ["PERCENTAGE", "A percentage (ex. `from-10%`)."],
  ["COLOR", "One of `COLORS` (ex. `bg-red-500`)."],
  ["ARBITRARY", "An arbitrary value (ex. `w-[3px]` or `w-(--width)`)."],
];
/** Arbitrary values for checking if a root that's not for lengths or colors accepts them. */
const arbitraryValues = ["[initial]", "[1]", "['a']", "[url(a)]", "[1s]", "[1deg]", "[1/1]", "[a]", "[1fr]"];
/** The customizations of the project that sort_tests_project.txt is for, which the Rust test has too. */
const projectPrefix = "tw";
const projectCss = `
@theme {
  --breakpoint-3xl: 120rem;
  --breakpoint-xs: 30rem;
  --container-8xl: 90rem;
  --color-brand: #f00;
  --color-brand-dark: #900;
  --text-huge: 4rem;
  --font-display: "Display";
  --font-weight-heavy: 950;
  --shadow-glow: 0 0 8px #fff;
  --radius-blob: 3rem;
  --spacing-gutter: 1.5rem;
  --ease-snappy: cubic-bezier(0.2, 0, 0, 1);
}
@custom-variant theme-midnight (&:where([data-theme="midnight"] *));
@custom-variant hocus (&:hover, &:focus);
@utility btn {
  display: inline-flex;
  padding: 1rem;
}
@utility tab-* {
  tab-size: --value(integer);
}
`;

const design = await loadDesign("");
const sortKeys = new Map<string, SortKey | undefined>();
const modifierKinds = new Map<string, ModifierKind>();
const colorValues: string[] = getThemeNames("--color");
const lineHeightValues: string[] = getThemeNames("--leading");
const properties = new Set<string>();
const utilities = getUtilities();
const propertyIndexes = getPropertyIndexes();
const themeValues = await getThemeValues();
const allSortKeys = getAllSortKeys();
const variants = getVariants();

writeFileSync(new URL("../src/tailwind/tables.rs", import.meta.url), getTablesText());
writeFileSync(new URL("../src/tailwind/sort_tests.txt", import.meta.url), getSortTestsText());
writeFileSync(new URL("../src/tailwind/sort_tests_project.txt", import.meta.url), await getProjectSortTestsText());

// deno-lint-ignore no-explicit-any
function loadDesign(css: string, importOptions = ""): Promise<any> {
  return __unstable__loadDesignSystem(`@import "tailwindcss"${importOptions};\n${css}`, {
    base: ".",
    loadStylesheet: (id: string, base: string) => {
      const path = require.resolve(id === "tailwindcss" ? "tailwindcss/index.css" : id);
      return Promise.resolve({ path, base, content: readFileSync(path, "utf8") });
    },
  });
}

/** Gets the names of the theme variables with the prefix sorted for searching. */
function getThemeNames(namespace: string) {
  const names: string[] = Array.from(design.theme.namespace(namespace).keys()).filter((name): name is string =>
    name != null
  );
  if (names.length === 0) {
    throw new Error(`Expected theme variables for ${namespace}.`);
  }
  return names.sort(compareText);
}

function getTablesText() {
  const exact = Array.from(utilities.exact.keys()).sort(compareText);
  const roots = Array.from(utilities.roots.entries()).sort((a, b) => compareText(a[0], b[0]));
  const position = (className: string) => getPosition(getSortKey(className)!);
  const valueSets = Array.from(new Set(roots.map(([, root]) => root.values.join(" ")))).sort(compareText);
  let text = `//! Order of the utilities and variants in Tailwind CSS ${tailwindVersion}.\n`;
  text += "//!\n";
  text += "//! Generated by scripts/generate_tailwind_tables.ts. Do not edit this file.\n\n";
  text += "/// What Tailwind orders utilities by, which is the indexes of the CSS properties a utility\n";
  text += "/// sets in Tailwind's property order along with how many declarations it has.\n";
  text += "///\n";
  text += "/// This has the ones of the utilities in the tables below in the order Tailwind sorts them.\n";
  text += "/// The position of a utility is the index of its entry doubled plus one, which leaves the\n";
  text += "/// even numbers for utilities that sort between two entries.\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const SORT_KEYS: &[(&[u16], u16)] = &[\n${
    allSortKeys.map(key => `  (&[${key.order.join(", ")}], ${key.count}),\n`).join("")
  }];\n\n`;
  text += "/// What may follow the slash after a utility (ex. the `50` in `bg-red-500/50`).\n";
  text += "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n";
  text += "pub enum Modifier {\n";
  text += "  /// Nothing.\n";
  text += "  None,\n";
  text += "  /// Only an arbitrary value (ex. `ease-in/[50%]`).\n";
  text += "  Arbitrary,\n";
  text += "  /// An opacity (ex. `bg-red-500/50`).\n";
  text += "  Opacity,\n";
  text += "  /// A line height (ex. `text-sm/6`).\n";
  text += "  LineHeight,\n";
  text += "  /// Anything (ex. `bg-linear-to-r/oklch`).\n";
  text += "  Any,\n";
  text += "}\n\n";
  text += "/// Utilities that are matched on their entire text along with their position and modifier.\n";
  text += "///\n";
  text += "/// This has the utilities that don't have a value (ex. `flex`) and the ones whose position or\n";
  text += "/// modifier differs from their root's default one (ex. `text-sm`, which is not a color).\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const UTILITIES: &[(&str, u16, Modifier)] = &[\n${
    exact.map(name => `  (${quote(name)}, ${position(name)}, Modifier::${getModifierKind(name)}),\n`).join("")
  }];\n\n`;
  text += "/// The root of utilities that are followed by a value (ex. `text` in `text-red-500`).\n";
  text += "pub struct UtilityRoot {\n";
  text += "  pub name: &'static str,\n";
  text += "  /// Position and modifier for most of the named values (ex. `border-red-500`).\n";
  text += "  pub default: (u16, Modifier),\n";
  text += "  /// Position and modifier for a number (ex. `border-3`).\n";
  text += "  pub number: (u16, Modifier),\n";
  text += "  /// Position and modifier for an arbitrary length (ex. `text-[14px]`).\n";
  text += "  pub arbitrary_length: (u16, Modifier),\n";
  text += "  /// Position and modifier for an arbitrary color (ex. `text-[#fff]`).\n";
  text += "  pub arbitrary_color: (u16, Modifier),\n";
  text += "  /// The kinds of values that are valid, which is a combination of the `VALUE_` constants.\n";
  text += "  pub value_kinds: u8,\n";
  text += "  /// The other values that are valid.\n";
  text += "  pub values: &'static [&'static str],\n";
  text += "}\n\n";
  valueKinds.forEach(([name, description], index) => {
    text += `/// ${description}\n`;
    text += `pub const VALUE_${name}: u8 = ${1 << index};\n`;
  });
  text += "\n";
  valueSets.forEach((values, index) => {
    text += "#[rustfmt::skip]\n";
    text += `const VALUES_${index}: &[&str] = &[${
      values.split(" ").filter(v => v.length > 0).map(quote).join(", ")
    }];\n`;
  });
  text += "\n";
  text += "/// Roots of the utilities that are followed by a value, where the ones that may be negative\n";
  text += "/// also have an entry that starts with a dash (ex. `-m` for `-m-4`).\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const UTILITY_ROOTS: &[UtilityRoot] = &[\n${
    roots.map(([name, root]) => {
      const entry = (className: string) => `(${position(className)}, Modifier::${getModifierKind(className)})`;
      const kinds = root.valueKinds.map(kind => `VALUE_${kind}`).join(" | ") || "0";
      return `  UtilityRoot { name: ${quote(name)}, default: ${entry(root.default)}, number: ${
        entry(root.number)
      }, arbitrary_length: ${entry(root.arbitraryLength)}, arbitrary_color: ${
        entry(root.arbitraryColor)
      }, value_kinds: ${kinds}, values: VALUES_${valueSets.indexOf(root.values.join(" "))} },\n`;
    }).join("")
  }];\n\n`;
  text +=
    "/// The names of the colors in Tailwind's theme, which are the values of the roots that have `VALUE_COLOR`.\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const COLORS: &[&str] = &[${colorValues.map(quote).join(", ")}];\n\n`;
  text += "/// The names of the line heights in Tailwind's theme.\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const LINE_HEIGHTS: &[&str] = &[${lineHeightValues.map(quote).join(", ")}];\n\n`;
  text += "/// Roots of the utilities whose value may be the name of a theme variable with a certain prefix\n";
  text += "/// (ex. `text-huge` when there's a `--text-huge`) along with the position and modifier they then have.\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const UTILITY_THEME_VALUES: &[(&str, &str, u16, Modifier)] = &[\n${
    themeValues.map(([root, namespace, key, modifier]) =>
      `  (${quote(root)}, ${quote(namespace)}, ${getPosition(key)}, Modifier::${modifier}),\n`
    ).join("")
  }];\n\n`;
  text += "/// CSS properties along with their index in Tailwind's property order.\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const PROPERTIES: &[(&str, u16)] = &[\n${
    propertyIndexes.map(([name, index]) => `  (${quote(name)}, ${index}),\n`).join("")
  }];\n\n`;
  text += "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n";
  text += "pub enum VariantKind {\n";
  text += "  /// Doesn't have a value (ex. `hover`).\n";
  text += "  Static,\n";
  text += "  /// May be followed by a value (ex. `data` in `data-active`).\n";
  text += "  Functional,\n";
  text += "  /// Is followed by another variant (ex. `group` in `group-hover`).\n";
  text += "  Compound,\n";
  text += "}\n\n";
  text += "/// How variants that have the same position are ordered.\n";
  text += "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n";
  text += "pub enum VariantValueOrder {\n";
  text += "  /// By their root then value.\n";
  text += "  None,\n";
  text += "  /// From the smallest value to the largest (ex. the width of the breakpoint in `min-md`).\n";
  text += "  Ascending,\n";
  text += "  /// From the largest value to the smallest (ex. the width of the breakpoint in `max-md`).\n";
  text += "  Descending,\n";
  text += "}\n\n";
  text += "/// What the value of a variant may be other than an arbitrary one.\n";
  text += "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n";
  text += "pub enum VariantValues {\n";
  text += "  /// Anything (ex. `data-active`).\n";
  text += "  Any,\n";
  text += "  /// A whole number (ex. `nth-3`).\n";
  text += "  Integer,\n";
  text += "  /// One that's in `VARIANT_VALUES` or the theme (ex. `max-md`).\n";
  text += "  Known,\n";
  text += "}\n\n";
  text += "/// Roots of the variants along with their position, the values that may follow\n";
  text += "/// them, and if they may have a modifier (ex. the `item` in `group-hover/item`).\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const VARIANTS: &[(&str, VariantKind, u16, VariantValueOrder, VariantValues, bool)] = &[\n${
    variants.roots.map(v =>
      `  (${
        quote(v.root)
      }, VariantKind::${v.kind}, ${v.order}, VariantValueOrder::${v.valueOrder}, VariantValues::${v.values}, ${v.hasModifier}),\n`
    ).join("")
  }];\n\n`;
  text += "/// Variants that are ordered by their value along with that value.\n";
  text += "#[rustfmt::skip]\n";
  text += `pub const VARIANT_VALUES: &[(&str, &str)] = &[\n${
    variants.values.map(([name, value]) => `  (${quote(name)}, ${quote(value)}),\n`).join("")
  }];\n`;
  return text;
}

function getUtilities() {
  const staticNames = new Set<string>(design.utilities.keys("static"));
  const functionalRoots = new Set<string>(design.utilities.keys("functional"));
  const classNames: string[] = design.getClassList()
    .map(([name]: [string]) => name)
    .filter((name: string) => isValid(name));
  const classNamesByRoot = new Map<string, string[]>();
  const exact = new Set<string>();
  for (const name of classNames) {
    const root = staticNames.has(name) ? undefined : findRoot(name);
    if (root == null) {
      exact.add(name);
    } else {
      classNamesByRoot.set(root, [...classNamesByRoot.get(root) ?? [], name]);
    }
  }

  const roots = new Map<string, UtilityRoot>();
  // negative utilities have their own roots because not everything may be negative
  for (const root of [...functionalRoots, ...Array.from(functionalRoots, root => `-${root}`)]) {
    // the position of a value that's not known is the most common one of the known values
    const names = classNamesByRoot.get(root) ?? [];
    const counts = new Map<string, { name: string; count: number }>();
    for (const name of names) {
      const key = getPositionAndModifier(name);
      const entry = counts.get(key) ?? { name, count: 0 };
      entry.count++;
      counts.set(key, entry);
    }
    const arbitraryLength = [`${root}-[1px]`].find(name => isValid(name));
    const arbitraryColor = [`${root}-[#000]`].find(name => isValid(name));
    const arbitraryOther = arbitraryValues.map(value => `${root}-${value}`).find(name => isValid(name));
    const valueKinds = valueKinds_(root, arbitraryLength ?? arbitraryColor ?? arbitraryOther);
    const defaultName = Array.from(counts.values()).sort((a, b) => b.count - a.count)[0]?.name
      ?? [arbitraryLength, arbitraryColor, arbitraryOther, ...bareValues.map(([, value]) => `${root}-${value}`)]
        .find(name => name != null && isValid(name));
    if (defaultName == null) {
      continue;
    }
    const numberName = bareValues.map(([, value]) => `${root}-${value}`).find(name => isValid(name)) ?? defaultName;
    const defaultKey = getPositionAndModifier(defaultName);
    const numberKey = getPositionAndModifier(numberName);
    const values: string[] = [];
    for (const name of names) {
      const value = name.substring(root.length + 1);
      const isNumber = valueKinds.some(kind => isValueOfKind(value, kind));
      if (getPositionAndModifier(name) !== (isNumber ? numberKey : defaultKey)) {
        exact.add(name);
      } else if (!isNumber) {
        values.push(value);
      }
    }
    if (colorValues.every(value => values.includes(value))) {
      valueKinds.push("COLOR");
    }
    roots.set(root, {
      default: defaultName,
      number: numberName,
      arbitraryLength: arbitraryLength ?? defaultName,
      arbitraryColor: arbitraryColor ?? defaultName,
      valueKinds,
      values: values.filter(value => !valueKinds.includes("COLOR") || !colorValues.includes(value)).sort(compareText),
    });
  }
  for (const [root, names] of classNamesByRoot) {
    if (!roots.has(root)) {
      names.forEach(name => exact.add(name));
    }
  }
  return { exact, roots };

  function findRoot(name: string) {
    if (name.startsWith("-")) {
      const root = findRoot(name.substring(1));
      return root == null ? undefined : `-${root}`;
    }
    if (functionalRoots.has(name)) {
      // has no value (ex. `border`), so needs to be matched on its entire text
      return undefined;
    }
    for (let index = name.lastIndexOf("-"); index > 0; index = name.lastIndexOf("-", index - 1)) {
      if (functionalRoots.has(name.substring(0, index))) {
        return name.substring(0, index);
      }
    }
    return undefined;
  }

  function valueKinds_(root: string, arbitraryName: string | undefined) {
    const kinds = bareValues.filter(([, value]) => isValid(`${root}-${value}`)).map(([kind]) => kind);
    if (arbitraryName != null) {
      kinds.push("ARBITRARY");
    }
    return kinds;
  }

  function getPositionAndModifier(name: string) {
    return JSON.stringify([getSortKey(name), getModifierKind(name)]);
  }
}

/** Gets if the value is one that a root with the kind of value accepts without it being listed. */
function isValueOfKind(value: string, kind: string) {
  switch (kind) {
    case "INTEGER":
      return /^(0|[1-9][0-9]*)$/.test(value);
    case "QUARTER":
      return /^(0|[1-9][0-9]*)\.(25|5|75)$/.test(value);
    case "DECIMAL":
      return /^(0|[1-9][0-9]*)\.[0-9]*[1-9]$/.test(value);
    case "FRACTION":
      return /^(0|[1-9][0-9]*)\/(0|[1-9][0-9]*)$/.test(value);
    case "PERCENTAGE":
      return /^(0|[1-9][0-9]*)%$/.test(value);
    case "ARBITRARY":
      return false;
    default:
      throw new Error(`Unknown kind ${kind}.`);
  }
}

// deno-lint-ignore no-explicit-any
function isValid(className: string, designSystem: any = design) {
  return getSortKey(className, designSystem) != null;
}

/** Gets what may follow the slash after the class name. */
// deno-lint-ignore no-explicit-any
function getModifierKind(className: string, designSystem: any = design): ModifierKind {
  const cacheKey = designSystem === design ? className : undefined;
  let result = cacheKey == null ? undefined : modifierKinds.get(cacheKey);
  if (result == null) {
    result = isValid(`${className}/zzzz`, designSystem)
      ? "Any"
      : isValid(`${className}/${lineHeightValues[0]}`, designSystem)
      ? "LineHeight"
      // not a whole number because that would make some values a fraction
      : isValid(`${className}/2.5`, designSystem)
      ? "Opacity"
      : isValid(`${className}/[50%]`, designSystem)
      ? "Arbitrary"
      : "None";
    if (cacheKey != null) {
      modifierKinds.set(cacheKey, result);
    }
  }
  return result;
}

function getPropertyIndexes() {
  const result: [string, number][] = [];
  for (const property of Array.from(properties).sort(compareText)) {
    const key = getSortKey(`[${property}:initial]`);
    if (key != null && key.order.length === 1) {
      result.push([property, key.order[0]]);
    }
  }
  return result;
}

/**
 * Gets the sort keys and modifiers of the utilities when their value is the name
 * of a theme variable, by adding a theme variable for each prefix.
 */
async function getThemeValues() {
  const name = (namespace: string) => `probe${namespace.replaceAll("-", "")}`;
  const probeDesign = await loadDesign(
    `@theme {\n${themeNamespaces.map(([namespace, value]) => `${namespace}${name(namespace)}: ${value};\n`).join("")}}`,
  );
  const result: [string, string, SortKey, ModifierKind][] = [];
  for (const root of utilities.roots.keys()) {
    for (const [namespace] of themeNamespaces) {
      const className = `${root}-${name(namespace)}`;
      const key = getSortKey(className, probeDesign);
      if (key != null) {
        result.push([root, namespace, key, getModifierKind(className, probeDesign)]);
      }
    }
  }
  return result.sort((a, b) => compareText(a[0], b[0]) || compareText(a[1], b[1]));
}

/** Gets the distinct sort keys of everything in the tables in the order Tailwind sorts them. */
function getAllSortKeys() {
  const keys = [
    ...utilities.exact.keys(),
    ...Array.from(utilities.roots.values()).flatMap(
      root => [root.default, root.number, root.arbitraryLength, root.arbitraryColor],
    ),
  ].map(className => getSortKey(className)!).concat(themeValues.map(([, , key]) => key));
  keys.sort(compareSortKeys);
  return keys.filter((key, index) => index === 0 || compareSortKeys(keys[index - 1], key) !== 0);
}

function getPosition(key: SortKey) {
  const index = allSortKeys.findIndex(other => compareSortKeys(key, other) === 0);
  if (index === -1) {
    throw new Error(`Expected to find the sort key ${JSON.stringify(key)}.`);
  }
  return index * 2 + 1;
}

// deno-lint-ignore no-explicit-any
function getSortKey(className: string, designSystem: any = design): SortKey | undefined {
  const cacheKey = designSystem === design ? className : undefined;
  if (cacheKey != null && sortKeys.has(cacheKey)) {
    return sortKeys.get(cacheKey);
  }
  let result: SortKey | undefined;
  for (const candidate of designSystem.parseCandidate(className)) {
    for (const { node, propertySort } of designSystem.compileAstNodes(candidate)) {
      collectProperties(node);
      if (result == null || compareSortKeys(propertySort, result) < 0) {
        result = propertySort;
      }
    }
  }
  if (cacheKey != null) {
    sortKeys.set(cacheKey, result);
  }
  return result;
}

// deno-lint-ignore no-explicit-any
function collectProperties(node: any) {
  if (node.kind === "declaration") {
    if (!node.property.startsWith("--")) {
      properties.add(node.property);
    }
  } else {
    node.nodes?.forEach(collectProperties);
  }
}

/** Compares the same way as the sort in Tailwind's `compileCandidates`. */
function compareSortKeys(a: SortKey, b: SortKey) {
  let offset = 0;
  while (offset < a.order.length && offset < b.order.length && a.order[offset] === b.order[offset]) {
    offset++;
  }
  if (offset === a.order.length && offset === b.order.length) {
    return b.count - a.count;
  }
  return ((a.order[offset] ?? Infinity) - (b.order[offset] ?? Infinity)) || (b.count - a.count);
}

function getVariants() {
  const kinds: Record<string, string> = { static: "Static", functional: "Functional", compound: "Compound" };
  const roots = Array.from<[string, { kind: string; order: number }]>(design.variants.variants.entries())
    .map(([root, { kind, order }]) => ({
      root,
      kind: kinds[kind],
      order,
      // variants that share a position are ordered by their value (ex. the width of a breakpoint)
      valueOrder: !design.variants.compareFns.has(order)
        ? "None"
        : root === "max" || root === "@max"
        ? "Descending"
        : "Ascending",
      values: kind === "static" || kind === "compound"
        ? "Known"
        : isValid(`${root === "@" ? "@" : `${root}-`}zzzz:flex`)
        ? "Any"
        : isValid(`${root}-7:flex`)
        ? "Integer"
        : "Known",
      // if what follows a slash after the variant is valid (ex. the `item` in `group-hover/item`)
      hasModifier: (root === "@" ? ["@sm/zzzz"] : [`${root}/zzzz`, `${root}-hover/zzzz`, `${root}-[1px]/zzzz`])
        .some(variant => isValid(`${variant}:flex`)),
    }))
    .sort((a, b) => compareText(a.root, b.root));
  const values: [string, string][] = [];
  const valuesByRoot = new Map<string, string[]>(
    design.getVariants().map((v: { name: string; values: string[] }) => [v.name, v.values]),
  );
  for (const { root, kind } of roots.filter(v => v.valueOrder !== "None")) {
    const themeKey = root.startsWith("@") ? "--container" : "--breakpoint";
    if (kind === "Static") {
      values.push([root, design.theme.resolveValue(root, [themeKey])]);
    }
    for (const value of valuesByRoot.get(root) ?? []) {
      values.push([root === "@" ? `@${value}` : `${root}-${value}`, design.theme.resolveValue(value, [themeKey])]);
    }
  }
  if (values.some(([, value]) => value == null)) {
    throw new Error("Expected every breakpoint to have a value.");
  }
  // ensure the directions above are right
  for (
    const [smaller, larger, expected] of [
      ["sm", "min-md", -1],
      ["max-sm", "max-md", 1],
      ["@sm", "@min-md", -1],
      ["@max-sm", "@max-md", 1],
    ] as const
  ) {
    if (Math.sign(design.variants.compare(design.parseVariant(smaller), design.parseVariant(larger))) !== expected) {
      throw new Error(`Unexpected order of ${smaller} and ${larger}.`);
    }
  }
  values.sort((a, b) => compareText(a[0], b[0]));
  return { roots, values };
}

function getSortTestsText() {
  const extraVariantNames = [
    ...["min-[400px]", "max-[600px]", "@[300px]", "@min-[20rem]", "@max-[40rem]"],
    ...["min-[30.5rem]", "max-[calc(100%-1rem)]", "@sm/sidebar"],
  ];
  const extraClassNames = [
    ...["text-[14px]", "text-[#bada55]", "bg-[#bada55]", "bg-[url(/img.png)]", "w-[32rem]", "p-[5px]"],
    ...["border-[3px]", "border-[#fff]", "[color:red]", "[mask-type:alpha]", "[--my-var:1px]", "m-[-2px]"],
    ...["grid-cols-[1fr_2fr]", "content-['hello']", "bg-red-500/50", "text-sm/6", "text-red-500/[0.5]"],
    ...["border-blue-300/25", "w-5/7", "-mt-[3px]", "text-(--my-color)", "text-(length:--my-size)"],
    ...["!p-4", "p-4!", "!flex", "text-red-500!", "foo", "bar", "unknown-class", "unknown:flex"],
  ];
  return createSortTestsText(design, 1000, 0.1, extraVariantNames, extraClassNames);
}

async function getProjectSortTestsText() {
  const projectDesign = await loadDesign(projectCss, ` prefix(${projectPrefix})`);
  const extraVariantNames = [
    ...["3xl", "xs", "min-3xl", "max-3xl", "max-xs", "@8xl", "@min-8xl", "@max-8xl"],
    ...["theme-midnight", "hocus", "group-hocus", "not-theme-midnight"],
  ];
  const extraClassNames = [
    ...["text-brand", "text-huge", "text-brand-dark", "bg-brand", "border-brand", "font-display", "font-heavy"],
    ...["shadow-glow", "shadow-brand", "rounded-blob", "p-gutter", "mx-gutter", "ease-snappy", "btn", "tab-4"],
    ...["tab-8", "ring-brand", "outline-brand", "decoration-brand", "stroke-brand", "max-w-8xl", "w-3xl"],
    ...["foo", "unknown-class"],
  ];
  const text = createSortTestsText(projectDesign, 500, 0.5, extraVariantNames, extraClassNames);
  const description = `# It's for a project that has the prefix \`${projectPrefix}\` and customizes Tailwind with:\n#${
    projectCss.trimEnd().replaceAll("\n", "\n# ")
  }\n#\n`;
  return text.replace("#\n", description);
}

function createSortTestsText(
  // deno-lint-ignore no-explicit-any
  designSystem: any,
  count: number,
  extraProbability: number,
  extraVariantNames: string[],
  extraClassNames: string[],
) {
  const random = createRandom(1);
  const pick = <T>(items: T[]) => items[Math.floor(random() * items.length)];
  const prefix = designSystem.theme.prefix == null ? "" : `${designSystem.theme.prefix}:`;
  // group the class names so that the many classes for colors don't crowd out the rest
  const classNameGroups = Array.from(
    Map.groupBy(
      design.getClassList().map(([name]: [string]) => name).filter((name: string) => getSortKey(name) != null),
      (name: string) => name.replace(/^-/, "").split("-")[0],
    ).values(),
  ) as string[][];
  const variantNames = [
    ...variants.roots.filter(v => v.kind === "Static").map(v => v.root),
    ...variants.values.map(([name]) => name),
    ...[
      "group-hover",
      "group-focus",
      "peer-checked",
      "peer-hover",
      "not-hover",
      "not-first",
      "has-checked",
      "in-focus",
    ],
    ...["group-hover/item", "peer-checked/name", "group-data-open", "not-data-active"],
    ...["aria-checked", "aria-busy", "data-active", "data-open", "data-[state=open]", "aria-[sort=ascending]"],
    ...["nth-3", "nth-last-2", "supports-grid", "supports-[display:grid]", "has-[img]"],
    ...["[&>*]", "[&:nth-child(3)]", "[@media(width>=600px)]"],
  ];
  // ones that may not be valid in order to check that what's not a Tailwind class isn't sorted as one
  const rootNames = Array.from(utilities.roots.keys());
  const maybeValues = [
    ...["banana", "7", "13", "1.5", "1.3", "3/7", "15%", "[7px]", "[#abc]", "(--x)", "px", "full", "auto"],
    ...["red-500", "none", "current", "04", "sm", "tight"],
  ];
  const maybeModifiers = ["50", "37", "2.5", "1.3", "7", "foo", "tight", "[0.3]", "(--x)", "oklch"];
  const maybeVariantNames = [
    ...["max-foo", "min-foo", "@foo", "has-foo", "group-foo", "not-foo", "in-foo", "peer-banana", "nth-foo"],
    ...["nth-7", "supports-foo", "data-foo", "aria-foo", "hover/foo", "sm/foo", "group-hover/foo", "@sm/foo"],
    ...["max-md/foo", "data-active/foo", "not-hover/foo", "data", "group", "min", "nth-[2n]", "min-7", "@7"],
    ...["", "[&]oops", "[&>*]/foo", "oops[&]", "[]"],
  ];
  const pickMaybeClassName = () =>
    random() < 0.5
      ? `${pick(rootNames)}-${pick(maybeValues)}`
      : `${pick(pick(classNameGroups))}/${pick(maybeModifiers)}`;
  const sizes = [2, 2, 3, 3, 4, 5, 6, 8, 12];
  let text = `# Class lists and how Tailwind CSS ${tailwindVersion} sorts them, separated by \`=>\`.\n`;
  text += "#\n";
  text += "# Generated by scripts/generate_tailwind_tables.ts. Do not edit this file.\n";
  for (let i = 0; i < count; i++) {
    const classes = Array.from({ length: pick(sizes) }, () => {
      const className = random() < extraProbability
        ? pick(extraClassNames)
        : random() < 0.2
        ? pickMaybeClassName()
        : pick(pick(classNameGroups));
      const variantCount = pick([0, 0, 0, 1, 1, 2, 3]);
      const classVariantNames = Array.from(
        { length: variantCount },
        () => pick(random() < extraProbability ? extraVariantNames : random() < 0.1 ? maybeVariantNames : variantNames),
      );
      return prefix + [...classVariantNames, className].join(":");
    });
    text += `${classes.join(" ")} => ${sortClasses(designSystem, classes).join(" ")}\n`;
  }
  return text;
}

/** Sorts the same way as prettier-plugin-tailwindcss. */
// deno-lint-ignore no-explicit-any
function sortClasses(designSystem: any, classes: string[]) {
  const ordered: [string, bigint | null][] = designSystem.getClassOrder(classes);
  ordered.sort(([, a], [, b]) => {
    if (a === b) return 0;
    if (a === null) return -1;
    if (b === null) return 1;
    return a < b ? -1 : 1;
  });
  const seen = new Set<string>();
  return ordered.filter(([name, order]) => {
    if (seen.has(name)) return false;
    if (order !== null) seen.add(name);
    return true;
  }).map(([name]) => name);
}

/** Creates a seeded random number generator so that the output is reproducible. */
function createRandom(seed: number) {
  return () => {
    seed = (seed + 0x6D2B79F5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

/** Compares by UTF-8 bytes like Rust does so that the tables can be binary searched. */
function compareText(a: string, b: string) {
  const encoder = new TextEncoder();
  const aBytes = encoder.encode(a);
  const bBytes = encoder.encode(b);
  for (let i = 0; i < Math.min(aBytes.length, bBytes.length); i++) {
    if (aBytes[i] !== bBytes[i]) {
      return aBytes[i] - bBytes[i];
    }
  }
  return aBytes.length - bBytes.length;
}

function quote(text: string) {
  return JSON.stringify(text);
}
