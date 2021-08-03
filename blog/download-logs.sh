#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

BUCKET=s3://cuddly-octo-palm-tree

LOGS=$(aws s3 ls $BUCKET/logs/ | awk '{print $4}')

if ! [ -d logs ]; then
    mkdir -p logs
fi

for log in $LOGS; do
    if [ -f logs/$log ]; then
        :
    else
        aws s3 cp $BUCKET/logs/$log logs/$log
    fi
done

cat logs/access* | gunzip | goaccess - -o report.html --log-format=COMBINED
