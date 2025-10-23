#!/bin/bash
set -e

cd /source/dprint-plugin-typescript

echo "Building..."
cargo build --release 2>&1 | tail -5

echo ""
echo "Running all tests..."
timeout 150 cargo test --test specs 2>&1 | tail -5
