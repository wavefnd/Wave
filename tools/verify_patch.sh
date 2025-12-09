#!/usr/bin/env bash
#
# Wave: Patch Verification Script
# This script applies a patch (via git am) and verifies:
# - DCO (Signed-off-by)
# - Build
# - Tests
# - Formatting
# - Lint (clippy)
#
# Usage:
#   ./tools/verify_patch.sh path/to/patch.patch
#
# Requirements:
#   - cargo
#   - rustfmt
#   - clippy
#

set -e

RED="\033[0;31m"
GREEN="\033[0;32m"
NC="\033[0m"

PATCH_FILE="$1"

if [ -z "$PATCH_FILE" ]; then
    echo -e "${RED}Error:${NC} No patch file provided."
    echo "Usage: ./tools/verify_patch.sh your_patch.patch"
    exit 1
fi

if [ ! -f "$PATCH_FILE" ]; then
    echo -e "${RED}Error:${NC} Patch file does not exist: $PATCH_FILE"
    exit 1
fi

echo "--------------------------------------------"
echo " Wave Patch Verification"
echo "--------------------------------------------"
echo "Patch file: $PATCH_FILE"
echo ""

# Create a temporary branch for patch testing
TEST_BRANCH="patch-verify-$(date +%s)"

echo "[1/7] Creating temporary branch: $TEST_BRANCH"
git checkout -b "$TEST_BRANCH" >/dev/null

echo "[2/7] Applying patch with git am..."
if ! git am "$PATCH_FILE"; then
    echo -e "${RED}Patch failed to apply.${NC}"
    git am --abort || true
    git checkout - >/dev/null
    git branch -D "$TEST_BRANCH" >/dev/null
    exit 1
fi

echo "[3/7] Checking DCO (Signed-off-by)..."
if ! git log -1 | grep -q "Signed-off-by:"; then
    echo -e "${RED}Error:${NC} Missing Signed-off-by line."
    git checkout - >/dev/null
    git branch -D "$TEST_BRANCH" >/dev/null
    exit 1
fi

echo "[4/7] Running cargo fmt --check..."
if ! cargo fmt --check; then
    echo -e "${RED}Formatting check failed.${NC}"
    git checkout - >/dev/null
    git branch -D "$TEST_BRANCH" >/dev/null
    exit 1
fi

echo "[5/7] Running cargo build..."
if ! cargo build --quiet; then
    echo -e "${RED}Build failed.${NC}"
    git checkout - >/dev/null
    git branch -D "$TEST_BRANCH" >/dev/null
    exit 1
fi

echo "[6/7] Running cargo test..."
if ! cargo test --quiet; then
    echo -e "${RED}Tests failed.${NC}"
    git checkout - >/dev/null
    git branch -D "$TEST_BRANCH" >/dev/null
    exit 1
fi

echo "[7/7] Running cargo clippy..."
if ! cargo clippy -- -D warnings; then
    echo -e "${RED}Clippy reported warnings/errors.${NC}"
    git checkout - >/dev/null
    git branch -D "$TEST_BRANCH" >/dev/null
    exit 1
fi

echo ""
echo -e "${GREEN}Patch verification SUCCESS!${NC}"
echo "Cleaning up..."

git checkout - >/dev/null
git branch -D "$TEST_BRANCH" >/dev/null

echo -e "${GREEN}Done.${NC}"
