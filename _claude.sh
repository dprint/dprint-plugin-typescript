#!/bin/bash
cd /source/dprint-plugin-typescript

echo "Building our plugin..."
cargo build --release 2>&1 | tail -3
echo ""

echo "Creating test file with nested parentheses..."
cat > _test_nested.ts << 'EOF'
const obj = {
    prop: (value),
    nested: ((other)),
};
EOF

echo ""
echo "=== Testing official dprint v0.95.11 ==="
cp _test_nested.ts _test1.ts
echo "Original:"
cat _test1.ts
echo ""

dprint fmt --plugins "https://plugins.dprint.dev/typescript-0.95.11.wasm" -- _test1.ts 2>&1 | grep -v "^Downloading"
echo "After 1st pass:"
cat _test1.ts
echo ""

dprint fmt --plugins "https://plugins.dprint.dev/typescript-0.95.11.wasm" -- _test1.ts 2>&1 | grep -v "^Downloading"
echo "After 2nd pass:"
cat _test1.ts
echo ""

echo "=== Testing our implementation with Disambiguation mode ==="
cp _test_nested.ts _test2.ts
echo '{"typescript": {"useParentheses": "disambiguation"}, "plugins": ["./target/release/libdprint_plugin_typescript.so"]}' > _test_config.json
dprint fmt --config _test_config.json -- _test2.ts 2>&1 | tail -1
echo "After 1st pass:"
cat _test2.ts
echo ""

dprint fmt --config _test_config.json -- _test2.ts 2>&1 | tail -1
echo "After 2nd pass:"
cat _test2.ts
echo ""

dprint fmt --config _test_config.json -- _test2.ts 2>&1 | tail -1
echo "After 3rd pass:"
cat _test2.ts
echo ""

echo "=== Testing our implementation with PreferNone mode ==="
cp _test_nested.ts _test3.ts
echo '{"typescript": {"useParentheses": "preferNone"}, "plugins": ["./target/release/libdprint_plugin_typescript.so"]}' > _test_config.json
dprint fmt --config _test_config.json -- _test3.ts 2>&1 | tail -1
echo "After 1st pass:"
cat _test3.ts
echo ""

dprint fmt --config _test_config.json -- _test3.ts 2>&1 | tail -1
echo "After 2nd pass:"
cat _test3.ts
echo ""

echo "=== Testing our implementation with Maintain mode ==="
cp _test_nested.ts _test4.ts
echo '{"typescript": {"useParentheses": "maintain"}, "plugins": ["./target/release/libdprint_plugin_typescript.so"]}' > _test_config.json
dprint fmt --config _test_config.json -- _test4.ts 2>&1 | tail -1
echo "After 1st pass:"
cat _test4.ts
echo ""

dprint fmt --config _test_config.json -- _test4.ts 2>&1 | tail -1
echo "After 2nd pass:"
cat _test4.ts

# Cleanup
rm -f _test_nested.ts _test1.ts _test2.ts _test3.ts _test4.ts _test_config.json
