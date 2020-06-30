#!/bin/bash
# to create patches use $ diff -u file1.rs file1.new.rs > file1.patch

# temporary in order to allow targeting wasm32-unknown-unknown
curl -L https://crates.io/api/v1/crates/sourcemap/6.0.0/download | tar xzf -
mv sourcemap-6.0.0 sourcemap
patch sourcemap/src/detector.rs patches/sourcemap_detector.patch
patch Cargo.toml patches/cargo.patch
