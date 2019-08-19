#!/usr/bin/env bash

set -euo pipefail

stack build
for f in $(ls test_data/input/); do
    export OUTPUT_PATH=$(mktemp)
    stack run < test_data/input/$f
    if diff $OUTPUT_PATH test_data/output/$f; then
        echo success
    else
        echo unexpected output
        exit -1
    fi
done
