#/usr/bin/env bash

set -euo pipefail

VERSION=$(git log -n1 --format=%cd-$(git rev-list --count HEAD)-%h --date=format:%Y%m%d --abbrev=8 .)
S3_FILE=s3://cuddly-octo-palm-tree/public/$VERSION.tar.gz

if aws s3 ls $S3_FILE; then
    echo "File already exists, no need to build."
else
    lein run
    ( cd public && tar czf ../public.tar.gz . )
    aws s3 cp public.tar.gz $S3_FILE
    rm public.tar.gz
fi
( cd tf && terraform apply -var="blog_version=$VERSION" )
