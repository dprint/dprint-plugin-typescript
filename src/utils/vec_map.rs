/// A faster map for when the mapping is being done over keys going from 0..n
pub struct VecMap<T> {
  inner: Vec<Option<T>>,
}

impl<T> VecMap<T> {
  pub fn with_capacity(capacity: usize) -> VecMap<T> {
    let mut inner = Vec::with_capacity(capacity);
    for _ in 0..capacity {
      inner.push(None);
    }

    VecMap { inner }
  }

  pub fn insert(&mut self, index: usize, value: T) {
    self.inner[index] = Some(value);
  }

  pub fn get(&self, index: usize) -> Option<&T> {
    self.inner.get(index).and_then(|value| value.as_ref())
  }

  pub fn remove(&mut self, index: usize) -> Option<T> {
    self.inner.get_mut(index).and_then(|value| value.take())
  }
}
