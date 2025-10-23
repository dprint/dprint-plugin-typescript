#!/bin/bash
set -e

cd /source/dprint-plugin-typescript

echo "Test case counts:"
echo "PreferNone: $(grep '^==' tests/specs/general/UseParentheses_PreferNone.txt | wc -l)"
echo "Maintain: $(grep '^==' tests/specs/general/UseParentheses_Maintain.txt | wc -l)"

echo ""
echo "Building and running tests..."
cargo build --release 2>&1 | tail -3
echo ""
timeout 150 cargo test --test specs 2>&1 | tail -5
