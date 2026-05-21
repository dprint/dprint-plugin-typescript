//! Merge pass for `module.mergeImports: true`.

/// A pure model of merge eligibility used in unit tests. The real entry
/// point operates on `ImportDecl` nodes (added in Task 8.2); this struct lets
/// us unit-test the rules without an AST.
#[derive(Clone)]
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
