#!/bin/bash
bumpVersion=$1
echo "Building Javascript..."
sh ./scripts/build.sh > /dev/null
echo "JS built!"
echo "Bumping $bumpVersion version"
npm --no-git-tag-version version $bumpVersion
git add . --quiet
git commit -m "update package json version" --quiet
spago bump-version $bumpVersion --no-dry-run
git push --follow-tags --quiet
npm publish
pulp publish