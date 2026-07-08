//! Stable partition of an import-group's nodes by classified group index.

/// Given a list of (group_index, original_index) pairs, return a new ordering
/// of original indices that:
///   1. groups items by group_index (in ascending order of group_index),
///   2. within each group, sorts using `cmp_within_group` (stable),
///   3. records the start index of each non-empty group as a boundary.
///
/// `cmp_within_group` may return `Equal` to mean "preserve source order".
pub fn partition_indices<F>(
  classified: &[(usize, usize)], // (group_index, original_index)
  num_groups: usize,
  mut cmp_within_group: F,
) -> (Vec<usize>, Vec<usize>)
where
  F: FnMut(usize, usize) -> std::cmp::Ordering,
{
  let mut buckets: Vec<Vec<usize>> = (0..num_groups).map(|_| Vec::new()).collect();
  for &(g, orig) in classified {
    buckets[g].push(orig);
  }

  for b in buckets.iter_mut() {
    b.sort_by(|&a, &b| cmp_within_group(a, b));
  }

  let mut ordered = Vec::with_capacity(classified.len());
  let mut boundaries = Vec::new();
  for b in buckets.into_iter() {
    if b.is_empty() {
      continue;
    }
    boundaries.push(ordered.len());
    ordered.extend(b);
  }
  (ordered, boundaries)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::cmp::Ordering;

  fn equal(_a: usize, _b: usize) -> Ordering {
    Ordering::Equal
  }

  #[test]
  fn three_groups_preserves_within_group_order_when_equal() {
    let input = vec![(0, 0), (2, 1), (1, 2), (0, 3), (2, 4)];
    let (ordered, boundaries) = partition_indices(&input, 3, equal);
    assert_eq!(ordered, vec![0, 3, 2, 1, 4]);
    assert_eq!(boundaries, vec![0, 2, 3]);
  }

  #[test]
  fn empty_buckets_omitted_from_boundaries() {
    let input = vec![(0, 0), (2, 1)];
    let (ordered, boundaries) = partition_indices(&input, 3, equal);
    assert_eq!(ordered, vec![0, 1]);
    assert_eq!(boundaries, vec![0, 1]);
  }

  #[test]
  fn within_group_sort_applies() {
    let input = vec![(0, 0), (0, 1), (0, 2)];
    let (ordered, _) = partition_indices(&input, 1, |a, b| b.cmp(&a));
    assert_eq!(ordered, vec![2, 1, 0]);
  }
}
