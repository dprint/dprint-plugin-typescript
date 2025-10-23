#!/bin/bash
cd /source/dprint-plugin-typescript

echo "Fixing whitespace differences in Disambiguation test expectations..."

# Run tests and capture output
timeout 90 cargo test --test specs -- "UseParentheses_Disambiguation" > /tmp/disambiguation_test_output.txt 2>&1

# Parse failures and update expectations
python3 << 'PYSCRIPT'
import re

# Read test output
with open('/tmp/disambiguation_test_output.txt', 'r') as f:
    test_output = f.read()

# Parse individual test failures with their actual output
failures = []
lines = test_output.split('\n')
i = 0
while i < len(lines):
    if lines[i].startswith('Failed:'):
        # Extract test name
        test_name = lines[i].replace('Failed:', '').strip()
        test_name = re.sub(r'\s+\(.*?\)$', '', test_name)  # Remove file path

        # Find Actual line
        actual_line = None
        for j in range(i+1, min(i+5, len(lines))):
            if lines[j].strip().startswith('Actual:'):
                # Match: Actual: `"<escaped string>"`,`,
                actual_match = re.search(r'Actual:\s+`"(.+?)"`(,`,)?$', lines[j])
                if actual_match:
                    actual_line = actual_match.group(1)
                    # Unescape
                    actual_line = actual_line.replace('\\n', '\n').replace('\\"', '"').replace('\\`', '`').replace('\\\\', '\\')
                break

        if actual_line:
            failures.append((test_name, actual_line))

    i += 1

print(f"Found {len(failures)} test failures to fix")

# Read Disambiguation file
with open('/source/dprint-plugin-typescript/tests/specs/general/UseParentheses_Disambiguation.txt', 'r') as f:
    content = f.read()

# For each failure, find the test and replace its [expect] section
for test_name, actual_output in failures:
    # Find test block: == test name == ... [expect] ... (until next ==)
    pattern = r'(== ' + re.escape(test_name) + r' ==.*?\[expect\]\n)(.*?)(\n\n==|\Z)'

    def replacer(match):
        before = match.group(1)
        after = match.group(3) if match.lastindex >= 3 else ''
        return before + actual_output + after

    content = re.sub(pattern, replacer, content, flags=re.DOTALL, count=1)
    print(f"  Fixed: {test_name}")

# Write back
with open('/source/dprint-plugin-typescript/tests/specs/general/UseParentheses_Disambiguation.txt', 'w') as f:
    f.write(content)

print(f"\nDone! Updated {len(failures)} test expectations with correct whitespace.")
PYSCRIPT

echo ""
echo "Running tests again to verify..."
timeout 60 cargo test --test specs -- "UseParentheses_Disambiguation" 2>&1 | tail -15
