#!/usr/bin/env bash

git log --no-merges --pretty=format:"  + %s (%an)" $1..$2 \
| sed 's/(Josh Berdine)//'
