#/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$DIR"

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
    # clean-up
    rm -rf public

    # resize images
    find content/img \
         -not -path '*/\.*' \
         -type f \
         -exec bash -c \
         "convert '{}' -resize 800x1000 '{}.out.jpg' && mv '{}.out.jpg' '{}'" \;

    # compile static site
    lein run

    # create tar
    ( cd public && tar czf ../public.tar.gz . )

    # push to S3
    aws s3 cp public.tar.gz $S3_FILE
    rm public.tar.gz
fi
( cd tf && terraform apply -var="blog_version=$VERSION" )
