#!/usr/bin/env bash
set -euo pipefail

# run_create_pr.sh - Automate GitHub PR creation for mcp-selenium-haskell
# Usage: ./run_create_pr.sh [PR_TITLE] [PR_BODY_FILE]

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "❌ Error: Not in a git repository"
    exit 1
fi

# Check if GitHub CLI is installed
if ! command -v gh &> /dev/null; then
    echo "❌ Error: GitHub CLI (gh) is not installed"
    echo "💡 Install with: nix develop"
    exit 1
fi

# Check if we're authenticated with GitHub
if ! gh auth status &> /dev/null; then
    echo "❌ Error: Not authenticated with GitHub"
    echo "💡 Run: gh auth login"
    exit 1
fi

# Get current branch
CURRENT_BRANCH=$(git branch --show-current)

# Check if we're on main/master
if [[ "$CURRENT_BRANCH" == "main" || "$CURRENT_BRANCH" == "master" ]]; then
    echo "❌ Error: Cannot create PR from main/master branch"
    echo "💡 Create a feature branch first: git checkout -b feature/your-feature"
    exit 1
fi

# Check if branch has unpushed commits
if ! git ls-remote --exit-code --heads origin "$CURRENT_BRANCH" &> /dev/null; then
    echo "📤 Branch '$CURRENT_BRANCH' doesn't exist on remote, pushing..."
    git push -u origin "$CURRENT_BRANCH"
elif ! git diff --quiet HEAD origin/"$CURRENT_BRANCH" 2>/dev/null; then
    echo "📤 Pushing latest changes to '$CURRENT_BRANCH'..."
    git push origin "$CURRENT_BRANCH"
fi

# Get PR title from argument or generate from branch name
if [[ $# -ge 1 && -n "$1" ]]; then
    PR_TITLE="$1"
else
    # Generate title from branch name and recent commits
    BRANCH_CLEAN=$(echo "$CURRENT_BRANCH" | sed 's/^[^/]*\///; s/-/ /g; s/\b\w/\u&/g')
    RECENT_COMMIT=$(git log -1 --pretty=format:'%s')
    PR_TITLE="$BRANCH_CLEAN"

    echo "💡 Generated PR title: $PR_TITLE"
    echo "💡 Recent commit: $RECENT_COMMIT"
fi

# Get PR body
PR_BODY=""
if [[ $# -ge 2 && -n "$2" && -f "$2" ]]; then
    PR_BODY_FILE="$2"
    if [[ ! -f "$PR_BODY_FILE" ]]; then
        echo "❌ Error: PR body file '$PR_BODY_FILE' not found"
        exit 1
    fi
    PR_BODY=$(cat "$PR_BODY_FILE")
    echo "📄 Using PR body from: $PR_BODY_FILE"
elif [[ -f "PR_DESCRIPTION.md" ]]; then
    PR_BODY=$(cat "PR_DESCRIPTION.md")
    echo "📄 Using PR body from: PR_DESCRIPTION.md"
else
    # Generate basic PR body from recent commits and branch changes
    echo "📝 Generating PR body from git log and changes..."

    # Get the base branch (usually main or master)
    BASE_BRANCH="main"
    if ! git show-ref --verify --quiet refs/remotes/origin/main; then
        BASE_BRANCH="master"
    fi

    # Generate PR body
    PR_BODY="## Changes

$(git log --oneline "$BASE_BRANCH".."$CURRENT_BRANCH" | sed 's/^/- /')

## Files Changed

$(git diff --name-only "$BASE_BRANCH".."$CURRENT_BRANCH" | sed 's/^/- /')

## Tests

- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Code linted successfully

## Checklist

- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] All tests passing
- [ ] Ready for review"
fi

echo ""
echo "🚀 Creating Pull Request..."
echo "📋 Title: $PR_TITLE"
echo "🌿 Branch: $CURRENT_BRANCH"
echo ""

# Create the PR
if gh pr create \
    --title "$PR_TITLE" \
    --body "$PR_BODY" \
    --base main \
    --head "$CURRENT_BRANCH"; then

    echo ""
    echo "✅ Pull Request created successfully!"
    echo ""

    # Show the PR
    gh pr view --web

else
    echo ""
    echo "❌ Failed to create Pull Request"
    echo "💡 Check if a PR already exists for this branch:"
    echo "   gh pr list --head $CURRENT_BRANCH"
    exit 1
fi
