Tests are added into the `~/tests/specs` folder.

They have the format:

```
== description goes here ==
const    u    =     2;

[expect]
const u = 2;
```

See [here](https://github.com/dprint/dprint/tree/main/crates/development#test-specs) for more details.

## Corpus test

The specs are small, so the corpus test also formats real files twice and reports the files that change when formatted again, whose formatted text doesn't parse, or that panic. It's skipped unless the `DPRINT_TS_CORPUS` environment variable is set to the corpus directories.

For example, with deno checked out at the commit in `tests/corpus_baselines/deno.txt`:

```sh
DPRINT_TS_CORPUS=../deno cargo test --release --test corpus -- --baseline tests/corpus_baselines/deno.txt
```

It only fails for files that aren't in the baseline, and lists the files in the baseline that no longer have problems so they can be removed. See `tests/corpus_test.rs` for the other options.
