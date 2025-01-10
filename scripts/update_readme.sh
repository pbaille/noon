#!/bin/bash

# Get the latest commit hash
latest_commit_hash=$(git log -1 --format="%H")

# Use sed to replace <LATEST_HASH> with the actual commit hash in README.md
sed "s/:sha \"[a-f0-9]\{40\}\"/:sha \"$latest_commit_sha\"/g" README.md
