#!/bin/bash
# Commit and push all changes

echo "Adding all changes to git..."
git add -A

echo ""
echo "Checking git status..."
git status

echo ""
echo "Creating commit..."
git commit -m "$(cat <<'COMMIT_MSG'
Add 4 new test cases for UseParentheses modes

- Test 1: nested assignments in logical expression
- Test 2: type assertion with optional chaining
- Test 3: type assertion with optional chaining in assignment
- Test 4: type assertion in method call

Added to all 3 modes:
- Disambiguation: keeps parentheses for disambiguation
- Maintain: keeps parentheses unchanged (same as input)
- PreferNone: removes parentheses where syntactically/semantically safe

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>
COMMIT_MSG
)"

echo ""
echo "Pushing to remote..."
git push origin merge

echo ""
echo "Done!"
