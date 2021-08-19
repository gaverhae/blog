#/usr/bin/env bash

set -euo pipefail

SHA=$(git log -n1 --format=%H .)
COUNT=$(git rev-list --count $SHA)
VERSION=$(git show -s --format=%cd-$COUNT-%h --date=format:%Y%m%d --abbrev=8 $SHA)
S3_FILE=s3://cuddly-octo-palm-tree/public/$VERSION.tar.gz

if ! [ -z "$(git status --porcelain)" ]; then
    echo "Not working on dirty worktree."
    exit 1
fi

if aws s3 ls $S3_FILE; then
    echo "File already exists, no need to build."
else
    rm -rf public
    lein run
    ( cd public && tar czf ../public.tar.gz . )
    aws s3 cp public.tar.gz $S3_FILE
    rm public.tar.gz
fi
( cd tf && terraform apply -var="blog_version=$VERSION" )
