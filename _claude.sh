#!/bin/bash
set -e

echo "Checking one specific test..."
timeout 150 cargo test --quiet 2>&1 | grep -A 10 "should keep parentheses around multi-line function expression property access" | head -20
