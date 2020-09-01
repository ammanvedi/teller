#!/bin/bash
bumpVersion=$1
echo "Building Javascript..."
sh ./scripts/build.sh > /dev/null
echo "JS built!"
echo "Bumping $bumpVersion version"
npm --no-git-tag-version version $bumpVersion
git add .
git commit -m "update package json version"
spago bump-version $bumpVersion --no-dry-run
git push --follow-tags
npm publish
pwd
pulp publish --no-push