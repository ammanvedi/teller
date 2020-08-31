#!/bin/bash
bumpVersion=$1
echo "Building Javascript..."
sh ./scripts/build.sh > /dev/null
echo "JS built!"
echo "Bumping $bumpVersion version"
npm --no-git-tag-version version $bumpVersion
spago bump-version $bumpVersion --no-dry-run
pulp publish