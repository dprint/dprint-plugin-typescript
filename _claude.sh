#!/bin/bash
cd /source/dprint-plugin-typescript

echo "=== Running all tests to verify simplification ==="
timeout 120 cargo test 2>&1 | grep -E "(test result:|tests passed)"
