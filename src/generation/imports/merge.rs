//! Merge pass for `module.mergeImports: true`.

/// A pure model of merge eligibility used in unit tests. The real entry
/// point operates on `ImportDecl` nodes (added in Task 8.2); this struct lets
/// us unit-test the rules without an AST.
#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub struct MergeCandidate {
  pub src: String,
  /// Canonicalized attribute fingerprint (`None` if no `with { ... }`).
  pub attrs: Option<String>,
  pub has_default: bool,
  pub default_name: Option<String>,
  pub has_ignore_comment: bool,
}

pub fn can_merge(a: &MergeCandidate, b: &MergeCandidate) -> bool {
  if a.src != b.src { return false; }
  if a.attrs != b.attrs { return false; }
  if a.has_ignore_comment || b.has_ignore_comment { return false; }
  if a.has_default && b.has_default && a.default_name != b.default_name {
    return false;
  }
  true
}

#[cfg(test)]
mod tests {
  use super::*;

  fn cand(src: &str) -> MergeCandidate {
    MergeCandidate {
      src: src.to_string(),
      attrs: None,
      has_default: false,
      default_name: None,
      has_ignore_comment: false,
    }
  }

  #[test]
  fn same_src_no_default_merges() {
    assert!(can_merge(&cand("./x"), &cand("./x")));
  }

  #[test]
  fn different_src_blocks() {
    assert!(!can_merge(&cand("./x"), &cand("./y")));
  }

  #[test]
  fn conflicting_defaults_block() {
    let a = MergeCandidate { has_default: true, default_name: Some("Foo".into()), ..cand("x") };
    let b = MergeCandidate { has_default: true, default_name: Some("Bar".into()), ..cand("x") };
    assert!(!can_merge(&a, &b));
  }

  #[test]
  fn same_default_merges() {
    let a = MergeCandidate { has_default: true, default_name: Some("Foo".into()), ..cand("x") };
    let b = MergeCandidate { has_default: true, default_name: Some("Foo".into()), ..cand("x") };
    assert!(can_merge(&a, &b));
  }

  #[test]
  fn different_attrs_block() {
    let mut a = cand("x");
    a.attrs = Some("type=json".into());
    let mut b = cand("x");
    b.attrs = Some("type=css".into());
    assert!(!can_merge(&a, &b));
  }

  #[test]
  fn dprint_ignore_blocks() {
    let mut a = cand("x");
    a.has_ignore_comment = true;
    assert!(!can_merge(&a, &cand("x")));
  }
}

/// Index-based bucket: either a single decl at index `i` or a merge of multiple decls.
#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum MergeBucket {
  Single(usize),
  Merged(Vec<usize>),
}

/// Compute merge buckets over a slice of decl metadata, in order.
/// MVP: only named-only imports from the same source merge.
#[allow(dead_code)]
pub fn compute_buckets(candidates: &[MergeCandidate], has_namespace: &[bool], has_named: &[bool]) -> Vec<MergeBucket> {
  let mut buckets: Vec<MergeBucket> = Vec::new();
  let mut current: Vec<usize> = Vec::new();
  for i in 0..candidates.len() {
    // MVP: only allow named-only imports from same source to merge.
    let mvp_eligible = !candidates[i].has_default
      && !has_namespace[i]
      && candidates[i].attrs.is_none()
      && !candidates[i].has_ignore_comment
      && has_named[i];

    if !mvp_eligible {
      flush_current(&mut buckets, &mut current);
      buckets.push(MergeBucket::Single(i));
      continue;
    }

    if current.is_empty() {
      current.push(i);
      continue;
    }
    let last = *current.last().unwrap();
    if can_merge(&candidates[last], &candidates[i]) {
      current.push(i);
    } else {
      flush_current(&mut buckets, &mut current);
      current.push(i);
    }
  }
  flush_current(&mut buckets, &mut current);
  buckets
}

#[allow(dead_code)]
fn flush_current(buckets: &mut Vec<MergeBucket>, current: &mut Vec<usize>) {
  match current.len() {
    0 => {}
    1 => buckets.push(MergeBucket::Single(current[0])),
    _ => buckets.push(MergeBucket::Merged(std::mem::take(current))),
  }
  current.clear();
}

#[cfg(test)]
mod bucket_tests {
  use super::*;

  fn c(src: &str) -> MergeCandidate {
    MergeCandidate {
      src: src.to_string(),
      attrs: None,
      has_default: false,
      default_name: None,
      has_ignore_comment: false,
    }
  }

  #[test]
  fn two_named_from_same_source_merge() {
    let cs = vec![c("x"), c("x")];
    let buckets = compute_buckets(&cs, &[false, false], &[true, true]);
    assert_eq!(buckets, vec![MergeBucket::Merged(vec![0, 1])]);
  }

  #[test]
  fn different_sources_stay_single() {
    let cs = vec![c("x"), c("y")];
    let buckets = compute_buckets(&cs, &[false, false], &[true, true]);
    assert_eq!(buckets, vec![MergeBucket::Single(0), MergeBucket::Single(1)]);
  }

  #[test]
  fn decl_with_default_excluded_from_mvp() {
    let mut a = c("x");
    a.has_default = true;
    a.default_name = Some("Foo".into());
    let cs = vec![a, c("x")];
    let buckets = compute_buckets(&cs, &[false, false], &[false, true]);
    // MVP excludes default-bearing decls.
    assert_eq!(buckets, vec![MergeBucket::Single(0), MergeBucket::Single(1)]);
  }

  #[test]
  fn three_in_a_row_merge() {
    let cs = vec![c("x"), c("x"), c("x")];
    let buckets = compute_buckets(&cs, &[false, false, false], &[true, true, true]);
    assert_eq!(buckets, vec![MergeBucket::Merged(vec![0, 1, 2])]);
  }
}
