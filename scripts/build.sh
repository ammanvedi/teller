#!/bin/bash
spago bundle-module -t dist/index.js -x
cp src/types/index.d.ts dist