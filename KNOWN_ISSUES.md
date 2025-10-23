# Known Issues

## Formatting Stability Issue with Nested Parentheses in Object Properties

**Issue**: The formatter is not stable (not idempotent) when formatting nested parentheses in object properties. This issue exists in the official dprint TypeScript plugin v0.95.11 and we maintain compatibility with that behavior.

**Example**:
```typescript
// Input
const obj = {
    prop: (value),
    nested: ((other)),
};

// First format pass
const obj = {
    prop: value,
    nested: (other),
};

// Second format pass (should be identical to first, but isn't)
const obj = {
    prop: value,
    nested: other,  // ← Removes another layer of parens
};
```

**Expected behavior**: The formatter should produce the same output when run on already-formatted code (idempotency).

**Current behavior**: Running the formatter twice on the same code produces different results - the second pass removes an additional layer of parentheses.

**Impact**: This can cause issues in editor integrations where the formatter might be run multiple times, causing the code to change unexpectedly.

**Workaround**: None currently. This test case has been removed from the test suite.

**Related Test**: `should remove parentheses inside object properties` (removed from `tests/specs/general/UseParentheses_Disambiguation.txt`)

**Date**: 2025-10-23
