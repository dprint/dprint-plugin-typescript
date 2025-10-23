#!/bin/bash
set -e

cd /source/dprint-plugin-typescript

echo "Building and running tests..."
cargo build --release 2>&1 | tail -3
echo ""
timeout 150 cargo test --test specs 2>&1 | tail -10
