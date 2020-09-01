#!/bin/bash
bumpVersion=$1
echo "Building Javascript..."
sh ./scripts/build.sh
echo "JS built!"
echo "Bumping $bumpVersion version"
npm --no-git-tag-version version $bumpVersion
git add .
git commit -m "update package json version"
spago bump-version $bumpVersion --no-dry-run
git push --follow-tags
npm publish
pulp docs
pulp publish --no-push