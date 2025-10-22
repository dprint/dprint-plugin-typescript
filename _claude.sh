#!/bin/bash
set -e

echo "Counting failures..."
timeout 150 cargo test --quiet 2>&1 | grep "^Failed:" | wc -l

echo ""
echo "First 10 failures..."
timeout 150 cargo test --quiet 2>&1 | grep "^Failed:" | head -10
