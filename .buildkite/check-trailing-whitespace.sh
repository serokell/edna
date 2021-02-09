#!/usr/bin/env bash

files=$(git ls-files -- . | xargs grep --exclude 'LICENSES/*' --exclude '*.patch' --files-with-matches --binary-files=without-match '[[:blank:]]$')
if [[ ! -z $files ]];then
    echo '  Files with trailing whitespace found:'
    for f in "${files[@]}"; do
        echo "  * $f"
    done
    exit 1
fi
