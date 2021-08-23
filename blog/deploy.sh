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
         "convert '{}' -resize 800x2000 '{}.out.jpg' && mv '{}.out.jpg' '{}'" \;

    # compile static site
    lein run

    # create tar
    ( cd public && tar czf ../public.tar.gz . )

    # push to S3
    aws s3 cp public.tar.gz $S3_FILE
    rm public.tar.gz
fi

jq -c --arg version $VERSION '. + [$version]' < tf/deployed > tf/deployed.tmp
mv tf/deployed.tmp tf/deployed

( cd tf && terraform apply -var="blog_version=$(cat deployed)" )

while ! { RESP=$(curl --fail \
                      --connect-to cuddly-octo-palm-tree.com:443:$(cat tf/out):443 \
                      https://cuddly-octo-palm-tree.com/version.txt) \
      && [[ "$VERSION" == "$RESP" ]] ;}; do
    echo $RESP
    sleep 5
done

jq -c 'reverse' < tf/deployed > tf/deployed.tmp
mv tf/deployed.tmp tf/deployed

( cd tf && terraform apply -var="blog_version=$(cat deployed)" )

jq -c '.[0:1]' < tf/deployed > tf/deployed.tmp
mv tf/deployed.tmp tf/deployed

( cd tf && terraform apply -var="blog_version=$(cat deployed)" )
