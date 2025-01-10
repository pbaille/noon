#!/bin/bash

# Get the latest commit hash
latest_commit_hash=$(git log -1 --format="%H")

# Use sed to replace <LATEST_HASH> with the actual commit hash in README.md
sed -i "s/<LATEST_HASH>/$latest_commit_hash/g" README.md
