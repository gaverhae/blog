#!/usr/bin/env bash

set -euo pipefail

lein uberjar
for f in $(ls ../test_data/input/); do
    export OUTPUT_PATH=$(mktemp)
    time java -jar target/uberjar/t-app-standalone.jar < ../test_data/input/$f
    if diff $OUTPUT_PATH ../test_data/output/$f; then
        echo success
    else
        echo unexpected output
        exit -1
    fi
done
