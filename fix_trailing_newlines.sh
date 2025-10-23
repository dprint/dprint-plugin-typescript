#!/bin/bash
cd /source/dprint-plugin-typescript

echo "Fixing extra trailing newlines in Disambiguation tests..."

python3 << 'PYSCRIPT'
import re

# Read the file
with open('/source/dprint-plugin-typescript/tests/specs/general/UseParentheses_Disambiguation.txt', 'r') as f:
    content = f.read()

# Replace any occurrence of [expect] ... code ... \n\n\n== with [expect] ... code ... \n\n==
# This fixes double blank lines between tests
fixed_content = re.sub(r'\[expect\]\n(.*?)\n\n\n(==)', r'[expect]\n\1\n\n\2', content, flags=re.DOTALL)

# Count how many fixes were made
original_count = content.count('\n\n\n==')
fixed_count = fixed_content.count('\n\n\n==')
fixes_made = original_count - fixed_count

print(f"Fixed {fixes_made} instances of extra trailing newlines")

# Write back
with open('/source/dprint-plugin-typescript/tests/specs/general/UseParentheses_Disambiguation.txt', 'w') as f:
    f.write(fixed_content)

print("Done!")
PYSCRIPT

echo ""
echo "Running Disambiguation tests to verify..."
timeout 60 cargo test --test specs -- "UseParentheses_Disambiguation" 2>&1 | tail -15
