#!/bin/bash
bumpVersion=$1
echo "Building Javascript..."
sh ./scripts/build.sh > /dev/null
echo "JS built!"
echo "Bumping $bumpVersion version"
spago bump-version $bumpVersion
npm version from-git
pulp publish